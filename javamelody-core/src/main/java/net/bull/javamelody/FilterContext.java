/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.io.File;
import java.io.IOException;
import java.security.CodeSource;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Pattern;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;

import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.JobInformations;
import net.bull.javamelody.internal.model.MBeans;
import net.bull.javamelody.internal.model.MavenArtifact;
import net.bull.javamelody.internal.model.Period;
import net.bull.javamelody.internal.model.SamplingProfiler;
import net.bull.javamelody.internal.model.UpdateChecker;
import net.bull.javamelody.internal.web.HttpCookieManager;
import net.bull.javamelody.internal.web.MailReport;
import net.bull.javamelody.internal.web.MonitoringController;

/**
 * Contexte du filtre http pour initialisation et destruction.
 * @author Emeric Vernat
 */
class FilterContext {
	private static final boolean MOJARRA_AVAILABLE = isMojarraAvailable();
	private static final boolean JPA2_AVAILABLE = isJpa2Available();

	private final String applicationType;
	private final Collector collector;
	private final Timer timer;
	private final SamplingProfiler samplingProfiler;
	private final TimerTask collectTimerTask;
	private final Set<ObjectName> jmxNames = new HashSet<ObjectName>();

	private static final class CollectTimerTask extends TimerTask {
		private final Collector collector;

		CollectTimerTask(Collector collector) {
			super();
			this.collector = collector;
		}

		/** {@inheritDoc} */
		@Override
		public void run() {
			// il ne doit pas y avoir d'erreur dans cette task
			collector.collectLocalContextWithoutErrors();
		}
	}

	FilterContext(final String applicationType) {
		super();
		assert applicationType != null;
		this.applicationType = applicationType;

		boolean initOk = false;
		this.timer = new Timer("javamelody"
				+ Parameters.getContextPath(Parameters.getServletContext()).replace('/', ' '),
				true);
		try {
			logSystemInformationsAndParameters();

			initLogs();

			if (Parameter.CONTEXT_FACTORY_ENABLED.getValueAsBoolean()) {
				MonitoringInitialContextFactory.init();
			}

			// si l'application a utilisé JdbcDriver avant d'initialiser ce filtre
			// (par exemple dans un listener de contexte), on doit récupérer son sqlCounter
			// car il est lié à une connexion jdbc qui est certainement conservée dans un pool
			// (sinon les requêtes sql sur cette connexion ne seront pas monitorées)
			// sqlCounter dans JdbcWrapper peut être alimenté soit par une datasource soit par un driver
			JdbcWrapper.SINGLETON.initServletContext(Parameters.getServletContext());
			if (!Parameters.isNoDatabase()) {
				JdbcWrapper.SINGLETON.rebindDataSources();
			} else {
				// si le paramètre no-database a été mis dans web.xml, des datasources jndi ont pu
				// être rebindées auparavant par SessionListener, donc on annule ce rebinding
				JdbcWrapper.SINGLETON.stop();
			}

			// initialisation du listener de jobs quartz
			if (JobInformations.QUARTZ_AVAILABLE) {
				JobGlobalListener.initJobGlobalListener();
			}

			if (MOJARRA_AVAILABLE) {
				JsfActionHelper.initJsfActionListener();
			}
			if (JPA2_AVAILABLE) {
				JpaPersistence.initPersistenceProviderResolver();
			}

			this.samplingProfiler = initSamplingProfiler();

			final List<Counter> counters = initCounters();
			final String application = Parameters.getCurrentApplication();
			this.collector = new Collector(application, counters, this.samplingProfiler);
			this.collectTimerTask = new CollectTimerTask(collector);

			initCollect();

			if (Parameter.JMX_EXPOSE_ENABLED.getValueAsBoolean()) {
				initJmxExpose();
			}

			UpdateChecker.init(timer, collector, applicationType);

			if (Parameters.getServletContext().getServerInfo().contains("Google App Engine")) {
				// https://issuetracker.google.com/issues/72216727
				final String fontConfig = System.getProperty("java.home")
						+ "/lib/fontconfig.Prodimage.properties";
				if (new File(fontConfig).exists()) {
					System.setProperty("sun.awt.fontconfig", fontConfig);
				}
			}

			initOk = true;
		} finally {
			if (!initOk) {
				// si exception dans initialisation, on annule la création du timer
				// (sinon tomcat ne serait pas content)
				timer.cancel();
				LOG.debug("JavaMelody init failed");
			}
		}
	}

	private static List<Counter> initCounters() {
		// liaison des compteurs : les contextes par thread du sqlCounter ont pour parent le httpCounter
		final Counter sqlCounter = JdbcWrapper.SINGLETON.getSqlCounter();
		final Counter httpCounter = new Counter(Counter.HTTP_COUNTER_NAME, "dbweb.png", sqlCounter);
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, "error.png");
		errorCounter.setMaxRequestsCount(250);

		final Counter jpaCounter = MonitoringProxy.getJpaCounter();
		final Counter ejbCounter = MonitoringProxy.getEjbCounter();
		final Counter springCounter = MonitoringProxy.getSpringCounter();
		final Counter guiceCounter = MonitoringProxy.getGuiceCounter();
		final Counter servicesCounter = MonitoringProxy.getServicesCounter();
		final Counter strutsCounter = MonitoringProxy.getStrutsCounter();
		final Counter jsfCounter = MonitoringProxy.getJsfCounter();
		final Counter logCounter = LoggingHandler.getLogCounter();
		final Counter jspCounter = JspWrapper.getJspCounter();
		final List<Counter> counters;
		if (JobInformations.QUARTZ_AVAILABLE) {
			final Counter jobCounter = JobGlobalListener.getJobCounter();
			counters = Arrays.asList(httpCounter, sqlCounter, jpaCounter, ejbCounter, springCounter,
					guiceCounter, servicesCounter, strutsCounter, jsfCounter, jspCounter,
					errorCounter, logCounter, jobCounter);
		} else {
			counters = Arrays.asList(httpCounter, sqlCounter, jpaCounter, ejbCounter, springCounter,
					guiceCounter, servicesCounter, strutsCounter, jsfCounter, jspCounter,
					errorCounter, logCounter);
		}

		setRequestTransformPatterns(counters);
		final String displayedCounters = Parameter.DISPLAYED_COUNTERS.getValue();
		if (displayedCounters == null) {
			// par défaut, les compteurs http, sql, error et log (et ceux qui sont utilisés) sont affichés
			httpCounter.setDisplayed(true);
			sqlCounter.setDisplayed(!Parameters.isNoDatabase());
			errorCounter.setDisplayed(true);
			logCounter.setDisplayed(true);
			jpaCounter.setDisplayed(jpaCounter.isUsed());
			ejbCounter.setDisplayed(ejbCounter.isUsed());
			springCounter.setDisplayed(springCounter.isUsed());
			guiceCounter.setDisplayed(guiceCounter.isUsed());
			servicesCounter.setDisplayed(servicesCounter.isUsed());
			strutsCounter.setDisplayed(strutsCounter.isUsed());
			jsfCounter.setDisplayed(jsfCounter.isUsed());
			jspCounter.setDisplayed(jspCounter.isUsed());
		} else {
			setDisplayedCounters(counters, displayedCounters);
		}
		LOG.debug("counters initialized");
		return counters;
	}

	private static void setRequestTransformPatterns(List<Counter> counters) {
		for (final Counter counter : counters) {
			// le paramètre pour ce nom de compteur doit exister
			final Parameter parameter = Parameter
					.valueOfIgnoreCase(counter.getName() + "_TRANSFORM_PATTERN");
			if (parameter.getValue() != null) {
				final Pattern pattern = Pattern.compile(parameter.getValue(),
						Pattern.MULTILINE | Pattern.DOTALL);
				counter.setRequestTransformPattern(pattern);
			}
		}
	}

	private static void setDisplayedCounters(List<Counter> counters, String displayedCounters) {
		for (final Counter counter : counters) {
			if (counter.isJobCounter()) {
				// le compteur "job" a toujours displayed=true s'il est présent,
				// même s'il n'est pas dans la liste des "displayedCounters"
				counter.setDisplayed(true);
			} else {
				counter.setDisplayed(false);
			}
		}
		if (!displayedCounters.isEmpty()) {
			for (final String displayedCounter : displayedCounters.split(",")) {
				final String displayedCounterName = displayedCounter.trim();
				boolean found = false;
				for (final Counter counter : counters) {
					if (displayedCounterName.equalsIgnoreCase(counter.getName())) {
						counter.setDisplayed(true);
						found = true;
						break;
					}
				}
				if (!found) {
					throw new IllegalArgumentException("Unknown counter: " + displayedCounterName);
				}
			}
		}
	}

	private void initCollect() {
		try {
			Class.forName("org.jrobin.core.RrdDb");
			// il a parfois été observé "ClassNotFoundException: org.jrobin.core.RrdException"
			// dans tomcat lors de l'arrêt du serveur à l'appel de JRobin.stop()
			Class.forName("org.jrobin.core.RrdException");
		} catch (final ClassNotFoundException e) {
			LOG.debug("jrobin classes unavailable: collect of data is disabled");
			HttpCookieManager.setDefaultRange(Period.TOUT.getRange());
			// si pas de jar jrobin, alors pas de collecte et période "Tout" par défaut
			return;
		}

		if (collector.isStorageUsedByMultipleInstances()) {
			LOG.info(I18N.getString("storage_used_by_multiple_instances"));
		}

		try {
			JRobin.initBackendFactory(timer);
		} catch (final IOException e) {
			LOG.warn(e.toString(), e);
		}
		final int resolutionSeconds = Parameters.getResolutionSeconds();
		final int periodMillis = resolutionSeconds * 1000;
		// on schedule la tâche de fond
		timer.schedule(collectTimerTask, periodMillis, periodMillis);
		LOG.debug("collect task scheduled every " + resolutionSeconds + 's');

		// on appelle la collecte pour que les instances jrobin soient définies
		// au cas où un graph de la page de monitoring soit demandé de suite
		collector.collectLocalContextWithoutErrors();
		LOG.debug("first collect of data done");

		if (Parameter.MAIL_SESSION.getValue() != null
				&& Parameter.ADMIN_EMAILS.getValue() != null) {
			MailReport.scheduleReportMailForLocalServer(collector, timer);
			LOG.debug("mail reports scheduled for " + Parameter.ADMIN_EMAILS.getValue());
		}
	}

	private SamplingProfiler initSamplingProfiler() {
		if (Parameter.SAMPLING_SECONDS.getValue() != null) {
			final SamplingProfiler sampler;
			final String excludedPackagesParameter = Parameter.SAMPLING_EXCLUDED_PACKAGES
					.getValue();
			final String includedPackagesParameter = Parameter.SAMPLING_INCLUDED_PACKAGES
					.getValue();
			if (excludedPackagesParameter == null && includedPackagesParameter == null) {
				sampler = new SamplingProfiler();
			} else {
				sampler = new SamplingProfiler(excludedPackagesParameter,
						includedPackagesParameter);
			}
			final TimerTask samplingTimerTask = new TimerTask() {
				@Override
				public void run() {
					sampler.update();
				}
			};
			final long periodInMillis = Math
					.round(Double.parseDouble(Parameter.SAMPLING_SECONDS.getValue()) * 1000);
			this.timer.schedule(samplingTimerTask, 10000, periodInMillis);
			LOG.debug("hotspots sampling initialized");

			return sampler;
		}
		return null;
	}

	private static void initLogs() {
		// on branche le handler java.util.logging pour le counter de logs
		LoggingHandler.getSingleton().register();

		if (LOG.LOG4J_ENABLED) {
			// si log4j est disponible on branche aussi l'appender pour le counter de logs
			Log4JAppender.getSingleton().register();
		}

		if (LOG.LOG4J2_ENABLED) {
			// si log4j2 est disponible on branche aussi l'appender pour le counter de logs
			Log4J2Appender.getSingleton().register();
		}

		if (LOG.LOGBACK_ENABLED) {
			// si logback est disponible on branche aussi l'appender pour le counter de logs
			LogbackAppender.getSingleton().register();
		}
		LOG.debug("log listeners initialized");
	}

	private static boolean isMojarraAvailable() {
		try {
			Class.forName("com.sun.faces.application.ActionListenerImpl");
			return true;
		} catch (final Throwable e) { // NOPMD
			return false;
		}
	}

	private static boolean isJpa2Available() {
		try {
			Class.forName("javax.persistence.spi.PersistenceProviderResolverHolder");
			return true;
		} catch (final Throwable e) { // NOPMD
			return false;
		}
	}

	private void logSystemInformationsAndParameters() {
		// log les principales informations sur le système et sur les paramètres définis spécifiquement
		LOG.debug("OS: " + System.getProperty("os.name") + ' '
				+ System.getProperty("sun.os.patch.level") + ", " + System.getProperty("os.arch")
				+ '/' + System.getProperty("sun.arch.data.model"));
		LOG.debug("Java: " + System.getProperty("java.runtime.name") + ", "
				+ System.getProperty("java.runtime.version"));
		LOG.debug("Server: " + Parameters.getServletContext().getServerInfo());
		LOG.debug("Webapp context: " + Parameters.getContextPath(Parameters.getServletContext()));
		LOG.debug("JavaMelody version: " + Parameters.JAVAMELODY_VERSION);
		final String location = getJavaMelodyLocation();
		if (location != null) {
			LOG.debug("JavaMelody classes loaded from: " + location);
		}
		LOG.debug("Application type: " + applicationType);
		LOG.debug("Application version: " + MavenArtifact.getWebappVersion());
		LOG.debug("Host: " + Parameters.getHostName() + '@' + Parameters.getHostAddress());
		for (final Parameter parameter : Parameter.values()) {
			final String value = parameter.getValue();
			if (value != null && parameter != Parameter.ANALYTICS_ID) {
				if (parameter == Parameter.AUTHORIZED_USERS) {
					LOG.debug("parameter defined: " + Parameter.AUTHORIZED_USERS.getCode()
							+ "=*****");
				} else {
					LOG.debug("parameter defined: " + parameter.getCode() + '=' + value);
				}
			}
		}
	}

	private static String getJavaMelodyLocation() {
		final Class<FilterContext> clazz = FilterContext.class;
		final CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
		if (codeSource != null && codeSource.getLocation() != null) {
			String location = codeSource.getLocation().toString();
			// location contient le nom du fichier jar
			// (ou le nom du fichier de cette classe s'il y a un répertoire sans jar)
			final String clazzFileName = clazz.getSimpleName() + ".class";
			if (location.endsWith(clazzFileName)) {
				location = location.substring(0, location.length() - clazzFileName.length());
			}
			return location;
		}
		return null;
	}

	/**
	 * Registers CounterRequestMXBean beans for each of the enabled counters.
	 * The beans are registered under "net.bull.javamelody:type=CounterRequest,context=<webapp>,name=<counter name>" names.
	 * @author Alexey Pushkin
	 */
	private void initJmxExpose() {
		final String packageName = getClass().getName().substring(0,
				getClass().getName().length() - getClass().getSimpleName().length() - 1);
		String webapp = Parameters.getContextPath(Parameters.getServletContext());
		if (webapp.length() >= 1 && webapp.charAt(0) == '/') {
			webapp = webapp.substring(1);
		}
		final List<Counter> counters = collector.getCounters();
		final MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
		try {
			for (final Counter counter : counters) {
				if (!Parameters.isCounterHidden(counter.getName())) {
					final CounterRequestMXBean.CounterRequestMXBeanImpl mxBean = new CounterRequestMXBean.CounterRequestMXBeanImpl(
							counter);
					final ObjectName name = new ObjectName(
							packageName + ":type=CounterRequest,context=" + webapp + ",name="
									+ counter.getName());
					platformMBeanServer.registerMBean(mxBean, name);
					jmxNames.add(name);
				}
			}
			LOG.debug("JMX mbeans registered");
		} catch (final JMException e) {
			LOG.warn("failed to register JMX mbeans", e);
		}
	}

	void stopCollector() {
		// cette méthode est appelée par MonitoringFilter lorsqu'il y a un serveur de collecte
		if (collectTimerTask != null) {
			// on arrête juste la tâche de collecte, mais pas le timer, ni la tâche d'UpdateChecker ni la tâche de sampling
			collectTimerTask.cancel();
		}
		// arrêt du collector
		collector.stop();
	}

	void destroy() {
		try {
			try {
				if (collector != null) {
					new MonitoringController(collector, null).writeHtmlToLastShutdownFile();
				}
			} finally {
				//on rebind les dataSources initiales à la place des proxy
				JdbcWrapper.SINGLETON.stop();

				deregisterJdbcDriver();

				// on enlève l'appender de logback, log4j et le handler de java.util.logging
				deregisterLogs();

				// on enlève le listener de jobs quartz
				if (JobInformations.QUARTZ_AVAILABLE) {
					JobGlobalListener.destroyJobGlobalListener();
				}

				unregisterJmxExpose();
			}
		} finally {
			MonitoringInitialContextFactory.stop();

			// on arrête le thread du collector,
			// on persiste les compteurs pour les relire à l'initialisation et ne pas perdre les stats
			// et on vide les compteurs
			if (timer != null) {
				timer.cancel();
			}
			if (samplingProfiler != null) {
				samplingProfiler.clear();
			}
			if (collector != null) {
				collector.stop();
			}
			Collector.stopJRobin();
			Collector.detachVirtualMachine();
		}
	}

	private static void deregisterJdbcDriver() {
		// on désinstalle le driver jdbc s'il est installé
		// (mais sans charger la classe JdbcDriver pour ne pas installer le driver)
		final Class<FilterContext> classe = FilterContext.class;
		final String packageName = classe.getName().substring(0,
				classe.getName().length() - classe.getSimpleName().length() - 1);
		for (final Driver driver : Collections.list(DriverManager.getDrivers())) {
			if (driver.getClass().getName().startsWith(packageName)) {
				try {
					DriverManager.deregisterDriver(driver);
				} catch (final SQLException e) {
					// ne peut arriver
					throw new IllegalStateException(e);
				}
			}
		}
	}

	private static void deregisterLogs() {
		if (LOG.LOGBACK_ENABLED) {
			LogbackAppender.getSingleton().deregister();
		}
		if (LOG.LOG4J_ENABLED) {
			Log4JAppender.getSingleton().deregister();
		}
		LoggingHandler.getSingleton().deregister();
	}

	/**
	 * Unregisters CounterRequestMXBean beans.
	 */
	private void unregisterJmxExpose() {
		if (jmxNames.isEmpty()) {
			return;
		}
		try {
			final MBeanServer platformMBeanServer = MBeans.getPlatformMBeanServer();
			for (final ObjectName name : jmxNames) {
				platformMBeanServer.unregisterMBean(name);
			}
		} catch (final JMException e) {
			LOG.warn("failed to unregister JMX beans", e);
		}
	}

	Collector getCollector() {
		return collector;
	}

	Timer getTimer() {
		return timer;
	}
}

/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

/**
 * Informations sur Tomcat, sans code html de présentation.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'un serveur Tomcat, similaire à celui fourni dans le manager de Tomcat.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
final class TomcatInformations implements Serializable {
	// cette classe utilise la même technique avec les MBeans Tomcat que la webapp manager de Tomcat
	// http://svn.apache.org/repos/asf/tomcat/trunk/java/org/apache/catalina/manager/StatusManagerServlet.java
	// http://svn.apache.org/repos/asf/tomcat/trunk/java/org/apache/catalina/manager/StatusTransformer.java
	// http://svn.apache.org/repos/asf/tomcat/trunk/webapps/manager/xform.xsl

	private static final boolean TOMCAT_USED = System.getProperty("catalina.home") != null;

	private static final long serialVersionUID = -6145865427461051370L;

	@SuppressWarnings("all")
	private static final List<ObjectName> THREAD_POOLS = new ArrayList<ObjectName>();
	@SuppressWarnings("all")
	private static final List<ObjectName> GLOBAL_REQUEST_PROCESSORS = new ArrayList<ObjectName>();

	private final String name;
	private final int maxThreads;
	private final int currentThreadCount;
	private final int currentThreadsBusy;
	private final long bytesReceived;
	private final long bytesSent;
	private final int requestCount;
	private final int errorCount;
	private final long processingTime;
	private final long maxTime;

	private TomcatInformations(MBeanServer mBeanServer, ObjectName threadPool) throws JMException {
		super();
		name = threadPool.getKeyProperty("name");
		maxThreads = (Integer) mBeanServer.getAttribute(threadPool, "maxThreads");
		currentThreadCount = (Integer) mBeanServer.getAttribute(threadPool, "currentThreadCount");
		currentThreadsBusy = (Integer) mBeanServer.getAttribute(threadPool, "currentThreadsBusy");
		ObjectName grp = null;
		for (final ObjectName globalRequestProcessor : GLOBAL_REQUEST_PROCESSORS) {
			if (name.equals(globalRequestProcessor.getKeyProperty("name"))) {
				grp = globalRequestProcessor;
			}
		}
		if (grp != null) {
			bytesReceived = (Long) mBeanServer.getAttribute(grp, "bytesReceived");
			bytesSent = (Long) mBeanServer.getAttribute(grp, "bytesSent");
			requestCount = (Integer) mBeanServer.getAttribute(grp, "requestCount");
			errorCount = (Integer) mBeanServer.getAttribute(grp, "errorCount");
			processingTime = (Long) mBeanServer.getAttribute(grp, "processingTime");
			maxTime = (Long) mBeanServer.getAttribute(grp, "maxTime");
		} else {
			bytesReceived = 0;
			bytesSent = 0;
			requestCount = 0;
			errorCount = 0;
			processingTime = 0;
			maxTime = 0;
		}
	}

	static List<TomcatInformations> buildTomcatInformationsList() {
		if (!TOMCAT_USED) {
			return Collections.emptyList();
		}
		try {
			synchronized (THREAD_POOLS) {
				if (THREAD_POOLS.isEmpty() || GLOBAL_REQUEST_PROCESSORS.isEmpty()) {
					initMBeans();
				}
			}
			final MBeanServer mBeanServer = getMBeanServer();
			final List<TomcatInformations> tomcatInformationsList = new ArrayList<TomcatInformations>(
					THREAD_POOLS.size());
			// rq: le processor correspondant au threadPool peut se retrouver selon
			// threadPool.getKeyProperty("name").equals(globalRequestProcessor.getKeyProperty("name"))
			for (final ObjectName threadPool : THREAD_POOLS) {
				tomcatInformationsList.add(new TomcatInformations(mBeanServer, threadPool));
			}
			return tomcatInformationsList;
		} catch (final JMException e) {
			// n'est pas censé arriver
			throw new IllegalStateException(e);
		}
	}

	// visibilité package pour réinitialisation en test unitaire
	static void initMBeans() throws MalformedObjectNameException {
		// rq: en général, il y a 2 connecteurs (http et ajp 1.3) définis dans server.xml et donc
		// 2 threadPools et 2 globalRequestProcessors de même nom : http-8080 et jk-8009 (ajp13)
		final MBeanServer mBeanServer = getMBeanServer();
		final Set<ObjectInstance> threadPoolInstances = mBeanServer.queryMBeans(new ObjectName(
				"*:type=ThreadPool,*"), null);
		final List<ObjectName> threadPools = new ArrayList<ObjectName>(threadPoolInstances.size());
		for (final ObjectInstance oi : threadPoolInstances) {
			threadPools.add(oi.getObjectName());
		}
		final Set<ObjectInstance> globalRequestProcessorInstances = mBeanServer.queryMBeans(
				new ObjectName("*:type=GlobalRequestProcessor,*"), null);
		final List<ObjectName> globalRequestProcessors = new ArrayList<ObjectName>(
				globalRequestProcessorInstances.size());
		for (final ObjectInstance oi : globalRequestProcessorInstances) {
			globalRequestProcessors.add(oi.getObjectName());
		}

		THREAD_POOLS.clear();
		GLOBAL_REQUEST_PROCESSORS.clear();
		THREAD_POOLS.addAll(threadPools);
		GLOBAL_REQUEST_PROCESSORS.addAll(globalRequestProcessors);
	}

	/**
	 * Retourne le javax.management.MBeanServer en le créant si nécessaire.
	 * @return MBeanServer
	 */
	private static MBeanServer getMBeanServer() {
		final List<MBeanServer> mBeanServers = MBeanServerFactory.findMBeanServer(null);
		if (!mBeanServers.isEmpty()) {
			// il existe déjà un MBeanServer créé précédemment par Tomcat ou bien ci-dessous
			return mBeanServers.get(0);
		}
		final MBeanServer server = MBeanServerFactory.createMBeanServer();
		LOG.debug("javax.management.MBeanServer created");
		return server;
	}

	String getName() {
		return name;
	}

	int getMaxThreads() {
		return maxThreads;
	}

	int getCurrentThreadCount() {
		return currentThreadCount;
	}

	int getCurrentThreadsBusy() {
		return currentThreadsBusy;
	}

	long getBytesReceived() {
		return bytesReceived;
	}

	long getBytesSent() {
		return bytesSent;
	}

	int getRequestCount() {
		return requestCount;
	}

	int getErrorCount() {
		return errorCount;
	}

	long getProcessingTime() {
		return processingTime;
	}

	long getMaxTime() {
		return maxTime;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", maxThreads="
				+ getMaxThreads() + ", currentThreadCount=" + getCurrentThreadCount()
				+ ", currentThreadsBusy=" + getCurrentThreadsBusy() + ", bytesReceived="
				+ getBytesReceived() + ", bytesSent=" + getBytesSent() + ", requestCount="
				+ getRequestCount() + ", errorCount=" + getErrorCount() + ", processingTime="
				+ getProcessingTime() + ", maxTime=" + getMaxTime() + ']';
	}
}

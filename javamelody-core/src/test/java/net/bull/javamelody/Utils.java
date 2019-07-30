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
import java.util.HashSet;
import java.util.Map;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;

import org.jrobin.core.RrdBackendFactory;
import org.jrobin.core.RrdException;
import org.quartz.SchedulerException;
import org.quartz.impl.StdSchedulerFactory;

import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.JRobin;
import net.bull.javamelody.internal.model.RrdNioBackendFactory;

/**
 * Classe utilitaire pour les tests unitaires.
 * @author Emeric Vernat
 */
public final class Utils {
	private static final String SYSTEM_ACTIONS_PROPERTY_NAME = Parameters.PARAMETER_SYSTEM_PREFIX
			+ Parameter.SYSTEM_ACTIONS_ENABLED.getCode();

	private Utils() {
		super();
	}

	public static void setProperty(Parameter parameter, String value) {
		setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
	}

	public static void setProperty(String string, String value) {
		if (value == null) {
			System.getProperties().remove(string);
		} else {
			System.setProperty(string, value);
		}
	}

	public static void initialize() {
		for (final Object systemProperty : new HashSet<Object>(System.getProperties().keySet())) {
			if (systemProperty.toString().startsWith(Parameters.PARAMETER_SYSTEM_PREFIX)) {
				System.getProperties().remove(systemProperty.toString());
			}
		}
		JRobin.stop();
		if (isQuartzSchedulerStarted()) {
			try {
				// shutdown seems needed at the moment in order that this job does not run forever:
				// https://javamelody.ci.cloudbees.com/job/javamelody/
				StdSchedulerFactory.getDefaultScheduler().shutdown();
			} catch (final SchedulerException e) {
				throw new IllegalStateException(e);
			}
		}

		Parameters.initialize((FilterConfig) null);
		Parameters.initialize((ServletContext) null);
		Parameters.initJdbcDriverParameters(null, null);
		// pour avoir les informations sur les connections, l'initialisation de la classe JdbcWrapper
		// doit se faire avec les actions systèmes activées
		System.setProperty(SYSTEM_ACTIONS_PROPERTY_NAME, "true");
		JdbcWrapper.USED_CONNECTION_INFORMATIONS.clear();
		System.getProperties().remove(SYSTEM_ACTIONS_PROPERTY_NAME);
		new File(System.getProperty("java.io.tmpdir")).mkdirs();

		try {
			// we must initialize default factory before creating any rrd
			if (!RrdBackendFactory.getDefaultFactory().getFactoryName()
					.equals(RrdNioBackendFactory.FACTORY_NAME)) {
				RrdBackendFactory.registerAndSetAsDefaultFactory(new RrdNioBackendFactory());
			}
		} catch (final RrdException e) {
			throw new IllegalStateException(e);
		}
	}

	private static boolean isQuartzSchedulerStarted() {
		final Map<Thread, StackTraceElement[]> stackTraces = Thread.getAllStackTraces();
		for (final Thread thread : stackTraces.keySet()) {
			if (thread.getName().contains("Quartz")) {
				return true;
			}
		}
		return false;
	}
}

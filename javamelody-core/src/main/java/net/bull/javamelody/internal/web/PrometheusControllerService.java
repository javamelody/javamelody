/*
 * Copyright 2019 by Roland Praml
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

package net.bull.javamelody.internal.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

import net.bull.javamelody.PrometheusControllerFactory;
import net.bull.javamelody.internal.common.LOG;

/**
 * Helper class which discovers and stores the PrometheusControllerFactories with the ServiceLoader API. 
 *
 * @author Roland Praml, FOCONIS AG
 */
class PrometheusControllerService {
	private final static PrometheusControllerFactory[] FACTORIES;

	static {
		final List<PrometheusControllerFactory> list = new ArrayList<PrometheusControllerFactory>();
		try {
			final ServiceLoader<PrometheusControllerFactory> loader = ServiceLoader
					.load(PrometheusControllerFactory.class);

			for (PrometheusControllerFactory factory : loader) {
				list.add(factory);
			}
		} catch (ServiceConfigurationError se) {
			LOG.warn("Could not configure PrometheusControllerService", se);
		}

		LOG.debug("found PrometheusControllerServices: " + list.size());
		if (list.isEmpty()) {
			FACTORIES = null;
		} else {
			FACTORIES = list.toArray(new PrometheusControllerFactory[list.size()]);
		}
	}

	static void report(PrintWriter out) throws IOException {
		if (FACTORIES == null) {
			return;
		}
		for (PrometheusControllerFactory factory : FACTORIES) {
			factory.createController(out).report();
		}
	}
}

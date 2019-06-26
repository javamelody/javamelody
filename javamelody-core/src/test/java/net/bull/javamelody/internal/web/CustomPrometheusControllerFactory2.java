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

import net.bull.javamelody.PrometheusController;
import net.bull.javamelody.PrometheusControllerFactory;

/**
 * PrometheusControllerFactory #1 for test case.
 *
 * @author Roland Praml, FOCONIS AG
 */
public class CustomPrometheusControllerFactory2 implements PrometheusControllerFactory {

	@Override
	public PrometheusController createController(PrintWriter out) {
		return new PrometheusController(out, "custom2_") {

			@Override
			public void report() throws IOException {
				printHeader(MetricType.GAUGE, "diskspace", "disk space of drives");
				printDoubleWithFields("diskspace", "{letter=\"a\"}", 1.44D);
				printDoubleWithFields("diskspace", "{letter=\"c\"}", 300000D);
			}
		};
	}

}

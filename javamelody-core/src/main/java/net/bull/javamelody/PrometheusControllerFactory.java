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

package net.bull.javamelody;

import java.io.PrintWriter;

/**
 * PrometheusControllerFactories are discovered with the ServiceLoader API.
 * 
 * Specify one or more custom implementations as full qualified name(s) in 
 * `META-INF/services/net.bull.javamelody.PrometheusControllerFactory`
 *
 * @author Roland Praml, FOCONIS AG
 */
public interface PrometheusControllerFactory {

	/**
	 * Factory method, must return a new PrometheusController instance
	 * @param out the printWriter
	 * @return a new PrometheusController
	 */
	public PrometheusController createController(PrintWriter out);
}

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

import javax.ejb.Asynchronous;
import javax.interceptor.Interceptor;

/**
 * Intercepteur pour CDI & pour EJB 3.1 (Java EE 6+),
 * configuré automatiquement pour les beans et méthodes ayant l'annotation @{@link Asynchronous}.
 * @author Emeric Vernat
 */
@Interceptor
@Asynchronous
public class MonitoringAsynchronousCdiInterceptor extends MonitoringInterceptor {
	private static final long serialVersionUID = 1L;

	// note: it would be cool to automatically monitor methods having @Schedule or @Schedules like @Asynchronous,
	// without having to add @Monitored on the method, but we can't
}

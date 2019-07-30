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

import java.lang.reflect.Method;

import javax.interceptor.InvocationContext;

/**
 * Intercepteur pour EJB 3, alternatif à {@link MonitoringInterceptor}.
 * Si utilisé à place de {@link MonitoringInterceptor}, les noms des EJB monitorés
 * sont en fonction de la classe de l'EJB et non en fonction de la classe déclarant la méthode.
 * Cela est utile quand il est souhaité de monitorer les méthodes EJB d'une classe abstraite parente
 * en les distinguant selon le type précis de l'EJB fils.
 * @author Emeric Vernat
 */
public class MonitoringTargetInterceptor extends MonitoringInterceptor {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getRequestName(InvocationContext context) {
		final Method method = context.getMethod();
		final Object target = context.getTarget();
		return target.getClass().getSimpleName() + '.' + method.getName();
	}
}

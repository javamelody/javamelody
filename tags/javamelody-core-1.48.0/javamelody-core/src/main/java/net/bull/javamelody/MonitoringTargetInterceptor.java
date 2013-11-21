/*
 * Copyright 2008-2012 by Emeric Vernat
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
		final String requestName = target.getClass().getSimpleName() + '.' + method.getName();
		return requestName;
	}
}

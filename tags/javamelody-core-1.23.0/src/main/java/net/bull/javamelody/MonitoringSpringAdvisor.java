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

import org.springframework.aop.support.DefaultPointcutAdvisor;

/**
 * Advisor Spring dont l'advice est par défaut une instance de {@link MonitoringSpringInterceptor}.
 * Cet advisor simplifie la configuration dans un fichier de context Spring.
 * @author Emeric Vernat
 */
public class MonitoringSpringAdvisor extends DefaultPointcutAdvisor {
	private static final long serialVersionUID = 1241977525834332907L;

	/**
	 * Constructeur.
	 */
	public MonitoringSpringAdvisor() {
		super();
		setAdvice(new MonitoringSpringInterceptor());
		// ordre par rapport aux autres advisors/aspects (issue 32)
		setOrder(0);
	}
}

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

import org.springframework.aop.Pointcut;
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

	/**
	 * Constructeur.
	 * @param pointcut Pointcut
	 */
	public MonitoringSpringAdvisor(Pointcut pointcut) {
		this();
		setPointcut(pointcut);
	}
}

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

import java.util.List;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

/**
 * Enregistrement du context Spring.
 * @author Emeric Vernat
 */
public class SpringContext implements ApplicationContextAware {
	private static SpringContext singleton;

	private ApplicationContext context;

	/**
	 * Constructeur.
	 */
	public SpringContext() {
		super();
		initSingleton(this);
	}

	public static SpringContext getSingleton() {
		return singleton;
	}

	private static void initSingleton(SpringContext springContext) {
		singleton = springContext;
	}

	/** {@inheritDoc} */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		context = applicationContext;
	}

	/**
	 * @return List des noms de beans Spring.
	 */
	public List<String> getBeanDefinitionNames() {
		return List.of(context.getBeanDefinitionNames());
	}

	/**
	 * @param name Nom du bean
	 * @return Instance du bean
	 */
	public Object getBean(String name) {
		return context.getBean(name);
	}
}

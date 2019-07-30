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

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.NoInitialContextException;
import javax.naming.spi.InitialContextFactory;

/**
 * Expérimental: Factory d'InitialContext JNDI interposant des proxy du contexte,
 * des dataSources, des connexions et des statements jdbc en complément pour le
 * cas où le rebinding dans JNDI de JdbcWrapper.rebindDataSources ne fonctionne pas.
 * Basé sur une idée de Bahar Limaye
 * (http://www.theserverside.com/tt/articles/article.tss?l=InterceptingJNDIFilters
 * ou http://javapro.texterity.com/javapro/live2004/?pg=38)
 * @author Emeric Vernat
 */
public class MonitoringInitialContextFactory implements InitialContextFactory {
	// on sauvegarde la factory initiale
	// (org.apache.naming.java.javaURLContextFactory dans Tomcat6
	// avec un scheme "java" tel que défini dans NamingManager.getURLContext)
	private static String initialContextFactory;

	// et on la remplace par la nôtre
	static void init() {
		initialContextFactory = System.getProperty(Context.INITIAL_CONTEXT_FACTORY);
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY,
				MonitoringInitialContextFactory.class.getName());
	}

	static void stop() {
		if (MonitoringInitialContextFactory.class.getName()
				.equals(System.getProperty(Context.INITIAL_CONTEXT_FACTORY))) {
			// on remet l'ancienne valeur
			System.setProperty(Context.INITIAL_CONTEXT_FACTORY, initialContextFactory);
		}
	}

	/** {@inheritDoc} */
	@Override
	public Context getInitialContext(Hashtable<?, ?> environment) throws NamingException { // NOPMD
		try {
			final Class<?> clazz = Class.forName(initialContextFactory);
			final InitialContextFactory icf = (InitialContextFactory) clazz.newInstance();
			final Context context = icf.getInitialContext(environment);
			final JdbcWrapper jdbcWrapper = JdbcWrapper.SINGLETON;
			return jdbcWrapper.createContextProxy(context);
		} catch (final ClassNotFoundException e) {
			throw createNamingException(e);
		} catch (final InstantiationException e) {
			throw createNamingException(e);
		} catch (final IllegalAccessException e) {
			throw createNamingException(e);
		}
	}

	private static NoInitialContextException createNamingException(Exception e) {
		final NoInitialContextException ex = new NoInitialContextException(e.toString());
		ex.initCause(e);
		return ex;
	}
}

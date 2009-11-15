/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY, MonitoringInitialContextFactory.class
				.getName());
	}

	static void stop() {
		// on remet l'ancienne valeur
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY, initialContextFactory);
	}

	/** {@inheritDoc} */
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

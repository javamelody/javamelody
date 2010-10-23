/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *fv
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

import static org.junit.Assert.assertNotNull;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;

import org.junit.Test;

/**
 * Test unitaire de la classe TomcatInformations.
 * @author Emeric Vernat
 */
public class TestTomcatInformations {
	/**
	 * Implémentation du MBean ThreadPool.
	 * @author Emeric Vernat
	 */
	public static class ThreadPool implements ThreadPoolMBean {
		/** {@inheritDoc} */
		public int getmaxThreads() {
			return 200;
		}

		/** {@inheritDoc} */
		public int getcurrentThreadsBusy() {
			return 22;
		}

		/** {@inheritDoc} */
		public int getcurrentThreadCount() {
			return 42;
		}
	}

	/**
	 * Interface du MBean ThreadPool.
	 * @author Emeric Vernat
	 */
	public interface ThreadPoolMBean {
		/**
		 * attribut maxThreads.
		 * @return int
		 */
		int getmaxThreads();

		/**
		 * attribut currentThreadsBusy.
		 * @return int
		 */
		int getcurrentThreadsBusy();

		/**
		 * attribut currentThreadCount.
		 * @return int
		 */
		int getcurrentThreadCount();
	}

	/**
	 * Implémentation du MBean GlobalRequestProcessor.
	 * @author Emeric Vernat
	 */
	public static class GlobalRequestProcessor implements GlobalRequestProcessorMBean {
		/** {@inheritDoc} */
		public long getbytesReceived() {
			return 0;
		}

		/** {@inheritDoc} */
		public long getbytesSent() {
			return 100000;
		}

		/** {@inheritDoc} */
		public int getrequestCount() {
			return 100;
		}

		/** {@inheritDoc} */
		public int geterrorCount() {
			return 1;
		}

		/** {@inheritDoc} */
		public long getprocessingTime() {
			return 2000;
		}

		/** {@inheritDoc} */
		public long getmaxTime() {
			return 10000;
		}
	}

	/**
	 * Interface du MBean GlobalRequestProcessor.
	 * @author Emeric Vernat
	 */
	public interface GlobalRequestProcessorMBean {
		/**
		 * attribut bytesReceived.
		 * @return int
		 */
		long getbytesReceived();

		/**
		 * attribut bytesSent.
		 * @return int
		 */
		long getbytesSent();

		/**
		 * attribut requestCount.
		 * @return int
		 */
		int getrequestCount();

		/**
		 * attribut errorCount.
		 * @return int
		 */
		int geterrorCount();

		/**
		 * attribut processingTime.
		 * @return int
		 */
		long getprocessingTime();

		/**
		 * attribut maxTime.
		 * @return int
		 */
		long getmaxTime();
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testTomcatInformations() throws JMException {
		System.setProperty("catalina.home", "unknown");
		// ce premier appel crée un MBeanServer
		assertNotNull("buildTomcatInformationsList",
				TomcatInformations.buildTomcatInformationsList());
		final MBeanServer mBeanServer = MBeanServerFactory.findMBeanServer(null).get(0);
		mBeanServer.registerMBean(new ThreadPool(), new ObjectName(
				"Catalina:type=ThreadPool,name=http-8080"));
		// les appels suivants réutilise le MBeanServer créé
		assertNotNull("buildTomcatInformationsList",
				TomcatInformations.buildTomcatInformationsList());
		mBeanServer.registerMBean(new GlobalRequestProcessor(), new ObjectName(
				"Catalina:type=GlobalRequestProcessor,name=http-8080"));
		assertNotNull("buildTomcatInformationsList",
				TomcatInformations.buildTomcatInformationsList());
	}
}

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
package net.bull.javamelody.internal.model;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.servlet.ServletContext;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;

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
		@Override
		public int getmaxThreads() {
			return 200;
		}

		/** {@inheritDoc} */
		@Override
		public int getcurrentThreadsBusy() {
			return 22;
		}

		/** {@inheritDoc} */
		@Override
		public int getcurrentThreadCount() {
			return 42;
		}

		/** {@inheritDoc} */
		@Override
		public String[] getdummy() {
			return new String[] { "1", "2" };
		}

		/** {@inheritDoc} */
		@Override
		public Object gettoStringException() {
			return new Object() {
				/** {@inheritDoc} */
				@Override
				public String toString() {
					throw new IllegalStateException("test");
				}
			};
		}

		/** {@inheritDoc} */
		@Override
		public int[] getintArrayAsInJRockit() {
			return new int[] { 1 };
		}

		/** {@inheritDoc} */
		@Override
		public double[] getdoubleArrayAsInJRockit() {
			return new double[] { 1d };
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

		/**
		 * attribut dummy.
		 * @return String[]
		 */
		String[] getdummy();

		/**
		 * attribut toStringException.
		 * @return Object
		 */
		Object gettoStringException();

		/**
		 * @return int[]
		 */
		int[] getintArrayAsInJRockit();

		/**
		 * @return double[]
		 */
		double[] getdoubleArrayAsInJRockit();
	}

	/**
	 * Implémentation du MBean GlobalRequestProcessor.
	 * @author Emeric Vernat
	 */
	public static class GlobalRequestProcessor implements GlobalRequestProcessorMBean {
		private int requestCount = 100;

		/** {@inheritDoc} */
		@Override
		public long getbytesReceived() {
			return 0;
		}

		/** {@inheritDoc} */
		@Override
		public long getbytesSent() {
			return 100000;
		}

		/** {@inheritDoc} */
		@Override
		public int getrequestCount() {
			return requestCount;
		}

		/**
		 * setter.
		 * @param count int
		 */
		public void setrequestCount(int count) {
			this.requestCount = count;
		}

		/** {@inheritDoc} */
		@Override
		public int geterrorCount() {
			return 1;
		}

		/** {@inheritDoc} */
		@Override
		public long getprocessingTime() {
			return 2000;
		}

		/** {@inheritDoc} */
		@Override
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

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		Utils.setProperty(Parameter.SYSTEM_ACTIONS_ENABLED, Boolean.TRUE.toString());
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testTomcatInformations() throws JMException {
		System.setProperty("catalina.home", "unknown");
		// ce premier appel crée un MBeanServer
		TomcatInformations.initMBeans();
		assertNotNull("buildTomcatInformationsList",
				TomcatInformations.buildTomcatInformationsList());
		final MBeanServer mBeanServer = MBeans.getPlatformMBeanServer();
		final List<ObjectName> mBeans = new ArrayList<ObjectName>();
		mBeans.add(
				mBeanServer
						.registerMBean(new ThreadPool(),
								new ObjectName("Catalina:type=ThreadPool,name=http-8080"))
						.getObjectName());
		TomcatInformations.initMBeans();
		try {
			// les appels suivants réutilise le MBeanServer créé
			assertNotNull("buildTomcatInformationsList",
					TomcatInformations.buildTomcatInformationsList());
			mBeans.add(
					mBeanServer
							.registerMBean(new GlobalRequestProcessor(),
									new ObjectName(
											"Catalina:type=GlobalRequestProcessor,name=http-8080"))
							.getObjectName());
			TomcatInformations.initMBeans();
			assertNotNull("buildTomcatInformationsList",
					TomcatInformations.buildTomcatInformationsList());
			for (final TomcatInformations tomcatInformations : TomcatInformations
					.buildTomcatInformationsList()) {
				tomcatInformations.toString();
			}

			final Counter counter = new Counter("http", null);
			final Collector collector = new Collector("test", Arrays.asList(counter));
			final ServletContext context = createNiceMock(ServletContext.class);
			expect(context.getServerInfo()).andReturn("Mock").anyTimes();
			expect(context.getMajorVersion()).andReturn(3).anyTimes();
			expect(context.getMinorVersion()).andReturn(0).anyTimes();
			expect(context.getContextPath()).andReturn("/test").anyTimes();
			replay(context);
			Parameters.initialize(context);
			collector.collectLocalContextWithoutErrors();
			verify(context);
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
			TomcatInformations.initMBeans();
		}
	}
}

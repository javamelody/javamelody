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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe MBeans.
 * @author Emeric Vernat
 */
public class TestMBeans {
	private MBeans mbeans;
	private MBeanServer mBeanServer;
	private final List<ObjectName> mbeansList = new ArrayList<ObjectName>();

	/** Before.
	 * @throws JMException e */
	@Before
	public void setUp() throws JMException {
		Utils.initialize();
		mbeans = new MBeans();
		mBeanServer = MBeanServerFactory.findMBeanServer(null).get(0);
		final ObjectInstance mBean1 = mBeanServer.registerMBean(new ThreadPool(), new ObjectName(
				"Catalina:type=ThreadPool"));
		mbeansList.add(mBean1.getObjectName());
		final ObjectInstance mBean2 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("Catalina:type=GlobalRequestProcessor,name=http-8080"));
		mbeansList.add(mBean2.getObjectName());
		final ObjectInstance mBean3 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("jonas:j2eeType=Servlet"));
		mbeansList.add(mBean3.getObjectName());
		final ObjectInstance mBean4 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("notjonas:type=Servlet"));
		mbeansList.add(mBean4.getObjectName());
		final ObjectInstance mBean5 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("jboss.deployment:type=Servlet"));
		mbeansList.add(mBean5.getObjectName());
	}

	/** After.
	 * @throws JMException e */
	@After
	public void tearDown() throws JMException {
		for (final ObjectName registeredMBean : mbeansList) {
			mBeanServer.unregisterMBean(registeredMBean);
		}
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetTomcatThreadPools() throws JMException {
		assertNotNull("getTomcatThreadPools", mbeans.getTomcatThreadPools());
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetTomcatGlobalRequestProcessors() throws JMException {
		assertNotNull("getTomcatGlobalRequestProcessors", mbeans.getTomcatGlobalRequestProcessors());
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetAttribute() throws JMException {
		assertNotNull("getAttribute", mbeans.getAttribute(mbeansList.get(0), "currentThreadsBusy"));
	}

	/** Test. */
	@Test
	public void testGetMapObjectNamesByDomainAndFirstProperty() {
		assertNotNull("getMapObjectNamesByDomainAndFirstProperty",
				mbeans.getMapObjectNamesByDomainAndFirstProperty());
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetMBeanInfo() throws JMException {
		assertNotNull("getMBeanInfo", mbeans.getMBeanInfo(mbeansList.get(0)));
	}

	/** Test.
	 * @throws JMException e
	 * @throws IllegalAccessException e */
	@Test
	public void testGetAttributes() throws JMException, IllegalAccessException {
		final MBeanAttributeInfo[] attributes = mbeans.getMBeanInfo(mbeansList.get(0))
				.getAttributes();
		assertNotNull("getAttributes", mbeans.getAttributes(mbeansList.get(0), attributes));
		JdbcWrapperHelper.setFieldValue(attributes[attributes.length - 1], "name", "Password");
		assertNotNull("getAttributes", mbeans.getAttributes(mbeansList.get(0), attributes));
		JdbcWrapperHelper.setFieldValue(attributes[attributes.length - 1], "isRead", Boolean.FALSE);
		assertNotNull("getAttributes", mbeans.getAttributes(mbeansList.get(0), attributes));
	}

	/** Test. */
	@Test
	public void testGetConvertedAttribute() {
		final String firstMBean = mbeansList.get(0).toString();
		final String message = "getConvertedAttributes";
		assertNotNull(message, MBeans.getConvertedAttributes(firstMBean + ".maxThreads"));
		assertNotNull(
				message,
				MBeans.getConvertedAttributes(firstMBean + ".maxThreads|" + firstMBean
						+ ".maxThreads"));
		assertNotNull(message, MBeans.getConvertedAttributes(firstMBean + ".intArrayAsInJRockit"));
		assertNotNull(message,
				MBeans.getConvertedAttributes(firstMBean + ".doubleArrayAsInJRockit"));
		try {
			MBeans.getConvertedAttributes("Catalina:type=instanceNotFound.maxThreads");
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
		try {
			MBeans.getConvertedAttributes("n'importe quoi.maxThreads");
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
		try {
			MBeans.getConvertedAttributes(firstMBean + ".Password");
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
		try {
			MBeans.getConvertedAttributes("noAttribute");
		} catch (final IllegalArgumentException e) {
			assertNotNull("e", e);
		}
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetAttributeDescription() throws JMException {
		mbeans.getAttributeDescription("maxThreads", mbeans.getMBeanInfo(mbeansList.get(0))
				.getAttributes());
	}

	/** Test. */
	@Test
	public void testFormatMBeansDescription() {
		assertNull("mbeansDescription", mbeans.formatMBeansDescription(null));
		assertNotNull("mbeansDescription", mbeans.formatMBeansDescription("abc"));
		assertNotNull("mbeansDescription", mbeans.formatMBeansDescription("a  b     c"));
	}
}

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
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.StringWriter;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import net.bull.javamelody.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.TestTomcatInformations.ThreadPool;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HtmlMBeansReport.
 * @author Emeric Vernat
 */
public class TestHtmlMBeansReport {
	/**
	 * MBean sans attribut.
	 */
	public static class EmptyAttribute implements EmptyAttributeMBean {
		// rien
	}

	/**
	 * interface sans attribut.
	 */
	public interface EmptyAttributeMBean {
		// rien
	}

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	private static void assertNotEmptyAndClear(StringWriter writer) {
		assertTrue("rapport vide", writer.getBuffer().length() > 0);
		writer.getBuffer().setLength(0);
	}

	/** Test.
	 * @throws IOException e
	 * @throws JMException e */
	@Test
	public void testToHtml() throws IOException, JMException {
		assertNotNull("mbeans", new MBeans());
		final MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
		final List<ObjectName> mBeans = new ArrayList<ObjectName>();
		try {
			final ObjectInstance mBean1 = mBeanServer.registerMBean(new ThreadPool(),
					new ObjectName("Catalina:type=ThreadPool"));
			mBeans.add(mBean1.getObjectName());

			final ObjectInstance mBean2 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=http-8080"));
			mBeans.add(mBean2.getObjectName());

			final ObjectInstance mBean3 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=http-8090"));
			mBeans.add(mBean3.getObjectName());

			final ObjectInstance mBean4 = mBeanServer.registerMBean(new EmptyAttribute(),
					new ObjectName("java.lang:type=Object"));
			mBeans.add(mBean4.getObjectName());

			// on force à null une des descriptions de bean et une des descriptions d'attribut
			final MBeanInfo mbeanInfo = mBeanServer.getMBeanInfo(mBeans.get(0));
			JdbcWrapperHelper.setFieldValue(mbeanInfo, "description", null);
			final MBeanAttributeInfo mbeanAttributeInfo0 = mbeanInfo.getAttributes()[0];
			// même description que le nom de l'attribut
			JdbcWrapperHelper.setFieldValue(mbeanAttributeInfo0, "description", "maxThreads");
			final MBeanAttributeInfo mbeanAttributeInfo1 = mbeanInfo.getAttributes()[1];
			JdbcWrapperHelper.setFieldValue(mbeanAttributeInfo1, "description", null);

			// register another mbeanServer
			MBeanServerFactory.createMBeanServer("jboss");

			final StringWriter writer = new StringWriter();
			final HtmlMBeansReport report = new HtmlMBeansReport(writer);
			report.toHtml();
			assertNotEmptyAndClear(writer);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} finally {
			for (final ObjectName registeredMBean : mBeans) {
				mBeanServer.unregisterMBean(registeredMBean);
			}
		}
	}
}

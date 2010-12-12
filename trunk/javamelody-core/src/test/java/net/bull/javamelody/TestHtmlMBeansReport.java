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
import java.util.ArrayList;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
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
		final MBeanServer mBeanServer = MBeanServerFactory.findMBeanServer(null).get(0);
		final List<ObjectName> mBeans = new ArrayList<ObjectName>();
		try {
			mBeans.add(mBeanServer.registerMBean(new ThreadPool(),
					new ObjectName("Catalina:type=ThreadPool")).getObjectName());
			mBeans.add(mBeanServer.registerMBean(new GlobalRequestProcessor(),
					new ObjectName("Catalina:type=GlobalRequestProcessor,name=http-8080"))
					.getObjectName());

			// on force à null une des descriptions
			final MBeanInfo mbeanInfo = mBeanServer.getMBeanInfo(mBeans.get(0));
			JdbcWrapperHelper.setFieldValue(mbeanInfo, "description", null);

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

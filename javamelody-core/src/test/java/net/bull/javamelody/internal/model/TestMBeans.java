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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.model.MBeanNode.MBeanAttribute;
import net.bull.javamelody.internal.model.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.internal.model.TestTomcatInformations.ThreadPool;

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
		mBeanServer = MBeans.getPlatformMBeanServer();
		final ObjectInstance mBean1 = mBeanServer.registerMBean(new ThreadPool(),
				new ObjectName("Catalina:type=ThreadPool"));
		mbeansList.add(mBean1.getObjectName());
		final ObjectInstance mBean2 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("Catalina:type=GlobalRequestProcessor,name=http-8080"));
		mbeansList.add(mBean2.getObjectName());
		final ObjectInstance mBean3 = mBeanServer.registerMBean(new GlobalRequestProcessor(),
				new ObjectName("jboss.deployment:type=Servlet"));
		mbeansList.add(mBean3.getObjectName());
	}

	/** After.
	 * @throws JMException e */
	@After
	public void tearDown() throws JMException {
		for (final ObjectName registeredMBean : mbeansList) {
			mBeanServer.unregisterMBean(registeredMBean);
		}
	}

	/** Test. */
	@Test
	public void testGetTomcatThreadPools() {
		assertNotNull("getTomcatThreadPools", MBeansAccessor.getTomcatThreadPools());
	}

	/** Test. */
	@Test
	public void testGetTomcatGlobalRequestProcessors() {
		assertNotNull("getTomcatGlobalRequestProcessors",
				MBeansAccessor.getTomcatGlobalRequestProcessors());
	}

	@Test
	public void testGetThreadAllocatedBytes() {
		assertEquals("getThreadAllocatedBytes",
				Math.round(ThreadInformations.getCurrentThreadAllocatedBytes() / 10000d),
				Math.round((MBeansAccessor.getThreadAllocatedBytes(Thread.currentThread().getId())
						- 432L) / 10000d));
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetAttribute() throws JMException {
		assertNotNull("getAttribute", mbeans.getAttribute(mbeansList.get(0), "currentThreadsBusy"));
	}

	/** Test.
	 * @throws JMException e */
	@Test
	public void testGetAllMBeanNodes() throws JMException {
		final List<MBeanNode> allMBeanNodes = MBeans.getAllMBeanNodes();
		assertNotNull("getAllMBeanNodes", allMBeanNodes);
		for (final MBeanNode mbeanNode : allMBeanNodes) {
			assertNotNull("mbeanNode", mbeanNode);
			assertNotNull("toString", mbeanNode.toString());
		}
	}

	@Test
	public void testToString() {
		final MBeanNode mBeanNode = new MBeanNode("name", "description",
				Arrays.asList(new MBeanAttribute("name", "description", "formattedValue")));
		assertNotNull("mbeanNode", mBeanNode);
		assertNotNull("toString", mBeanNode.toString());
		assertNotNull("getAttributes", mBeanNode.getAttributes());
		for (final MBeanAttribute attribute : mBeanNode.getAttributes()) {
			assertNotNull("attribute", attribute);
			assertNotNull("toString", attribute.toString());
		}
	}

	/** Test. */
	@Test
	public void testGetConvertedAttribute() {
		final String firstMBean = mbeansList.get(0).toString();
		final String message = "getConvertedAttributes";
		assertNotNull(message, MBeans.getConvertedAttributes(firstMBean + ".maxThreads"));
		assertNotNull(message, MBeans
				.getConvertedAttributes(firstMBean + ".maxThreads|" + firstMBean + ".maxThreads"));
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
}

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

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.management.JMException;
import javax.management.MBeanServer;
import javax.management.ObjectInstance;
import javax.management.ObjectName;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.model.MBeanNode.MBeanAttribute;
import net.bull.javamelody.internal.model.TestTomcatInformations.GlobalRequestProcessor;
import net.bull.javamelody.internal.model.TestTomcatInformations.ThreadPool;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test unitaire de la classe MBeans.
 * @author Emeric Vernat
 */
class TestMBeans {
	private MBeans mbeans;
	private MBeanServer mBeanServer;
	private final List<ObjectName> mbeansList = new ArrayList<>();

	/** Before.
	 * @throws JMException e */
	@BeforeEach
	void setUp() throws JMException {
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
	@AfterEach
	void tearDown() throws JMException {
		for (final ObjectName registeredMBean : mbeansList) {
			mBeanServer.unregisterMBean(registeredMBean);
		}
	}

	/** Test. */
	@Test
	void testGetTomcatThreadPools() {
		assertNotNull(MBeansAccessor.getTomcatThreadPools(), "getTomcatThreadPools");
	}

	/** Test. */
	@Test
	void testGetTomcatGlobalRequestProcessors() {
		assertNotNull(MBeansAccessor.getTomcatGlobalRequestProcessors(), "getTomcatGlobalRequestProcessors");
	}

	@Test
	void testGetThreadAllocatedBytes() {
		assertEquals(Math.round(ThreadInformations.getCurrentThreadAllocatedBytes() / 10000d),
				Math.round((MBeansAccessor.getThreadAllocatedBytes(Thread.currentThread().getId()) - 432L) / 10000d),
				"getThreadAllocatedBytes");
	}

	/** Test.
	 * @throws JMException e */
	@Test
	void testGetAttribute() throws JMException {
		assertNotNull(mbeans.getAttribute(mbeansList.get(0), "currentThreadsBusy"), "getAttribute");
	}

	/** Test.
	 * @throws JMException e */
	@Test
	void testGetAllMBeanNodes() throws JMException {
		final List<MBeanNode> allMBeanNodes = MBeans.getAllMBeanNodes();
		assertNotNull(allMBeanNodes, "getAllMBeanNodes");
		for (final MBeanNode mbeanNode : allMBeanNodes) {
			assertNotNull(mbeanNode, "mbeanNode");
			assertNotNull(mbeanNode.toString(), "toString");
		}
	}

	@Test
	void testToString() {
		final MBeanNode mBeanNode = new MBeanNode("name", "description",
				List.of(new MBeanAttribute("name", "description", "formattedValue")));
		assertNotNull(mBeanNode, "mbeanNode");
		assertNotNull(mBeanNode.toString(), "toString");
		assertNotNull(mBeanNode.getAttributes(), "getAttributes");
		for (final MBeanAttribute attribute : mBeanNode.getAttributes()) {
			assertNotNull(attribute, "attribute");
			assertNotNull(attribute.toString(), "toString");
		}
	}

	/** Test. */
	@Test
	void testGetConvertedAttribute() {
		final String firstMBean = mbeansList.get(0).toString();
		final String message = "getConvertedAttributes";
		assertNotNull(message, MBeans.getConvertedAttributes(firstMBean + ".maxThreads"));
		assertNotNull(message, MBeans
				.getConvertedAttributes(firstMBean + ".maxThreads|" + firstMBean + ".maxThreads"));
		assertNotNull(message, MBeans.getConvertedAttributes(firstMBean + ".intArrayAsInJRockit"));
		assertNotNull(message,
				MBeans.getConvertedAttributes(firstMBean + ".doubleArrayAsInJRockit"));
		assertThrows(IllegalArgumentException.class, () ->
			MBeans.getConvertedAttributes("Catalina:type=instanceNotFound.maxThreads")
		);
		assertThrows(IllegalArgumentException.class, () ->
			MBeans.getConvertedAttributes("n'importe quoi.maxThreads")
		);
		assertThrows(IllegalArgumentException.class, () ->
			MBeans.getConvertedAttributes(firstMBean + ".Password")
		);
		assertThrows(IllegalArgumentException.class, () ->
			MBeans.getConvertedAttributes("noAttribute")
		);
	}

	private String find(String name1, String name2, String name3, String attributeName)
			throws JMException {
		for (final MBeanNode mBeanNode : MBeans.getAllMBeanNodes()) {
			for (final MBeanNode children : mBeanNode.getChildren()) {
				if (children.getName().equals(name1)) {
					for (final MBeanNode cc : children.getChildren()) {
						if (cc.getName().equals(name2)) {
							for (final MBeanNode ccc : cc.getChildren()) {
								if (ccc.getName().equals(name3)) {
									for (final MBeanAttribute aaa : ccc.getAttributes()) {
										if (aaa.getName().equals(attributeName)) {
											return aaa.getFormattedValue();
										}
									}
								}
							}
						}
					}
				}
			}
		}
		return null;
	}

	@Test
	void testGetConvertedAttribute_I18N() throws JMException {
		final ObjectInstance mBean1 = mBeanServer.registerMBean(new ThreadPool() {
			@Override
			public int getmaxThreads() {
				return 1234;
			}

		}, new ObjectName("Catalina:type=ThreadPool2"));
		mbeansList.add(mBean1.getObjectName());

		final String message = "testGetConvertedAttribute_I18N";
		I18N.bindLocale(Locale.US);
		assertEquals("1,234",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "maxThreads"),
				message);
		assertEquals("{committed=1, init=10, max=100, used=1,000}",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "memoryUsage"),
				message);
		I18N.bindLocale(Locale.FRANCE);
		assertEquals("1\u00a0234",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "maxThreads"),
				message);
		assertEquals("{committed=1, init=10, max=100, used=1\u00a0000}",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "memoryUsage"),
				message);
		I18N.bindLocale(Locale.FRENCH);
		assertEquals("1\u00a0234",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "maxThreads"),
				message);
		assertEquals("{committed=1, init=10, max=100, used=1\u00a0000}",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "memoryUsage"),
				message);
		I18N.bindLocale(Locale.ITALIAN);
		assertEquals("1.234",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "maxThreads"),
				message);
		assertEquals("{committed=1, init=10, max=100, used=1.000}",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "memoryUsage"),
				message);
		I18N.bindLocale(Locale.ITALY);
		assertEquals("1.234",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "maxThreads"),
				message);
		assertEquals("{committed=1, init=10, max=100, used=1.000}",
				find("Catalina", "ThreadPool2", "Catalina:type=ThreadPool2", "memoryUsage"),
				message);
	}
}

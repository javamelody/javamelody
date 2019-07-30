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

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;

import com.opensymphony.xwork2.ActionInvocation;
import com.opensymphony.xwork2.mock.MockActionProxy;

import net.bull.javamelody.internal.model.Counter;

/**
 * Test unitaire de la classe StrutsInterceptor.
 * @author Emeric Vernat
 */
public class TestStrutsInterceptor {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testGetStrutsCounter() {
		assertNotNull("getStrutsCounter", MonitoringProxy.getStrutsCounter());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testStruts() throws Exception {
		final Counter strutsCounter = MonitoringProxy.getStrutsCounter();
		strutsCounter.clear();
		final StrutsInterceptor strutsInterceptor = new StrutsInterceptor();
		final ActionInvocation invocation = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy = new MockActionProxy();
		proxy.setInvocation(invocation);
		proxy.setActionName("test.action");
		proxy.setMethod(null);
		proxy.setNamespace("testnamespace");
		expect(invocation.getProxy()).andReturn(proxy).anyTimes();

		replay(invocation);
		strutsCounter.setDisplayed(false);
		strutsInterceptor.intercept(invocation);
		final String requestsCount = "requestsCount";
		assertSame(requestsCount, 0, strutsCounter.getRequestsCount());

		strutsCounter.setDisplayed(true);
		strutsInterceptor.intercept(invocation);
		assertSame(requestsCount, 1, strutsCounter.getRequestsCount());
		verify(invocation);

		final ActionInvocation invocation2 = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy2 = new MockActionProxy();
		proxy2.setInvocation(invocation2);
		proxy2.setActionName("test2.action");
		proxy2.setMethod("execute");
		proxy2.setNamespace("testnamespace");
		expect(invocation2.getProxy()).andReturn(proxy2).anyTimes();

		replay(invocation2);
		strutsInterceptor.intercept(invocation2);
		assertSame(requestsCount, 2, strutsCounter.getRequestsCount());
		verify(invocation2);

		final ActionInvocation invocation3 = createNiceMock(ActionInvocation.class);
		final MockActionProxy proxy3 = new MockActionProxy();
		proxy3.setInvocation(invocation3);
		proxy3.setActionName("test3.action");
		proxy3.setMethod("testmethod");
		proxy3.setNamespace("testnamespace");
		expect(invocation3.getProxy()).andReturn(proxy3).anyTimes();
		expect(invocation3.invoke()).andThrow(new UnknownError("test d'erreur")).anyTimes();

		replay(invocation3);
		try {
			strutsInterceptor.intercept(invocation3);
		} catch (final UnknownError e) {
			assertNotNull("ok", e);
		}
		assertSame(requestsCount, 3, strutsCounter.getRequestsCount());
		verify(invocation3);
	}
}

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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Enumeration;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import com.sun.faces.application.ApplicationFactoryImpl;
import com.sun.faces.application.ApplicationImpl;
import com.sun.faces.config.InitFacesContext;

import jakarta.faces.FactoryFinder;
import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.event.ActionEvent;
import jakarta.faces.event.ActionListener;
import jakarta.servlet.ServletContext;

/**
 * Test unitaire pour JsfActionHelper et JsfActionListener.
 * @author Emeric Vernat
 */
public class TestJsf {
	public static class AppFactory extends ApplicationFactoryImpl {
		public AppFactory() {
			super();
			setApplication(new ApplicationImpl());
		}
	}

	@Test
	@Disabled
	// Test skipped because of IllegalArgumentException: Could not create type
	//	at net.bytebuddy.TypeCache.findOrInsert(TypeCache.java:170)
	public void testInitJsfActionListener() throws NoSuchMethodException, SecurityException,
			InvocationTargetException, IllegalAccessException {
		final ServletContext servletContext = createNiceMock(ServletContext.class);
		expect(servletContext.getInitParameter("com.sun.faces.enableTransitionTimeNoOpFlash"))
				.andReturn(null).anyTimes();
		final Enumeration<String> initParameterNames = Collections.emptyEnumeration();
		expect(servletContext.getInitParameterNames()).andReturn(initParameterNames).anyTimes();
		replay(servletContext);
		final InitFacesContext facesContext = new InitFacesContext(servletContext);
		final Method setter = FacesContext.class.getDeclaredMethod("setCurrentInstance",
				FacesContext.class);
		setter.setAccessible(true);
		setter.invoke(null, facesContext);
		FactoryFinder.setFactory(FactoryFinder.APPLICATION_FACTORY, AppFactory.class.getName());
		verify(servletContext);

		final ActionListener delegateActionListener = createNiceMock(ActionListener.class);
		FacesContext.getCurrentInstance().getApplication()
				.setActionListener(delegateActionListener);

		JsfActionHelper.initJsfActionListener();

		final UIComponent uiComponent = createNiceMock(UIComponent.class);
		final ActionEvent actionEvent = new ActionEvent(uiComponent);
		final ActionListener actionListener = FacesContext.getCurrentInstance().getApplication()
				.getActionListener();
		actionListener.processAction(actionEvent);
		MonitoringProxy.getJsfCounter().setDisplayed(false);
		actionListener.processAction(actionEvent);
		MonitoringProxy.getJsfCounter().setDisplayed(true);
	}
}

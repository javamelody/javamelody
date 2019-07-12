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

import com.google.inject.AbstractModule;
import com.google.inject.matcher.Matchers;

/**
 * Module Guice pour configurer l'intercepteur de monitoring utilisant l'annotation
 * {@link MonitoredWithGuice}, sur des classes et/ou sur des méthodes.<br/>
 * Ce module fait simplement:
 * <code><br/>
 *      // for annotated methods with MonitoredWithGuice<br/>
 *      bindInterceptor(Matchers.any(), Matchers.annotatedWith(MonitoredWithGuice.class),
 *				new MonitoringGuiceInterceptor());<br/>
 *      // and for annotated classes with MonitoredWithGuice<br/>
 *      bindInterceptor(Matchers.annotatedWith(MonitoredWithGuice.class), Matchers.any(),
 *				new MonitoringGuiceInterceptor());
 * </code>
 * @author Emeric Vernat
 */
public class MonitoringGuiceModule extends AbstractModule {
	/** {@inheritDoc} */
	@Override
	protected void configure() {
		// for annotated methods (of implementations) with MonitoredWithGuice
		final MonitoringGuiceInterceptor monitoringGuiceInterceptor = new MonitoringGuiceInterceptor();
		bindInterceptor(Matchers.any(), Matchers.annotatedWith(MonitoredWithGuice.class),
				monitoringGuiceInterceptor);
		// and for annotated classes (of implementations) with MonitoredWithGuice
		bindInterceptor(Matchers.annotatedWith(MonitoredWithGuice.class), Matchers.any(),
				monitoringGuiceInterceptor);
	}
}

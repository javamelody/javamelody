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
package net.bull.javamelody.internal.model; // NOPMD

import java.util.HashMap;
import java.util.Map;

import net.bull.javamelody.internal.model.SamplingProfiler.SampledMethod;

/**
 * Liste des alias <a href='http://x-stream.github.io/'>XStream</a> pour les conversions XML et JSON.
 * @author Emeric Vernat
 */
final class XStreamAlias {
	private XStreamAlias() {
		super();
	}

	static Map<String, Class<?>> getMap() {
		final Map<String, Class<?>> result = new HashMap<String, Class<?>>();
		result.put("counter", Counter.class);
		result.put("request", CounterRequest.class);
		result.put("requestContext", CounterRequestContext.class);
		result.put("javaInformations", JavaInformations.class);
		result.put("memoryInformations", MemoryInformations.class);
		result.put("tomcatInformations", TomcatInformations.class);
		result.put("threadInformations", ThreadInformations.class);
		result.put("heapHisto", HeapHistogram.class);
		result.put("connectionInformations", ConnectionInformations.class);
		result.put("classInfo", HeapHistogram.ClassInfo.class);
		result.put("sessionInformations", SessionInformations.class);
		result.put("sessionAttribute", SessionInformations.SessionAttribute.class);
		result.put("cacheInformations", CacheInformations.class);
		result.put("jobInformations", JobInformations.class);
		result.put("counterError", CounterError.class);
		result.put("method", SampledMethod.class);
		result.put("processInformations", ProcessInformations.class);
		result.put("databaseInformations", DatabaseInformations.class);
		result.put("jndiBinding", JndiBinding.class);
		result.put("mbeanNode", MBeanNode.class);
		result.put("attribute", MBeanNode.MBeanAttribute.class);
		return result;
	}
}

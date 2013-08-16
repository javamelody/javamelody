/*
 * Copyright 2008-2012 by Emeric Vernat
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
package net.bull.javamelody; // NOPMD

import java.util.HashMap;
import java.util.Map;

import net.bull.javamelody.SamplingProfiler.SampledMethod;

/**
 * Liste des alias XStream pour les conversions XML et JSON.
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
		result.put("attribute", SessionInformations.SessionAttribute.class);
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

/*
 * Copyright 2008-2021 by Emeric Vernat
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

import javax.management.JMException;
import javax.management.ObjectName;

/**
 * Selection of an attribute from an JMX-Bean to be counted. 
 * 
 * @author Sönke Küper
 */
public final class MBeanValueSelection {

	private final String name;
	private final ObjectName objectName;
	private final String attributeName;

	/**
	 * Creates an new {@link MBeanValueSelection}.
	 * @param name Name to be displayed in the ui.
	 * @param objectName {@link ObjectName JXM Object Name} from which the attribute is requested.
	 * @param attributeName The attribute name to be requested via jmx.
	 */
	public MBeanValueSelection(String name, ObjectName objectName, String attributeName) {
		this.name = name;
		this.objectName = objectName;
		this.attributeName = attributeName;
	}

	/**
	 * Returns the name for the value.
	 * @return String
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the Object-Name.
	 * @return {@link ObjectName}.
	 */
	public ObjectName getObjectName() {
		return objectName;
	}

	/**
	 * Returns the attribute name.
	 * @return String
	 */
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * Returns the value or <code>null</code> if attribute isn't a numeric value.
	 * @return Long
	 */
	public Long getValue() {
		try {
			Object value = MBeansAccessor.getAttribute(this.objectName, this.attributeName);

			if (value instanceof Number) {
				return ((Number) value).longValue();
			}
			return null;

		} catch (JMException e) {
			throw new IllegalStateException(e);
		}
	}

}

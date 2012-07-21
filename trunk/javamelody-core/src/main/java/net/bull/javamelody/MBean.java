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
package net.bull.javamelody;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

/**
 * Instance de données sérialisables représentant un MBean et construite à partir d'un ObjectName.
 * @author Emeric Vernat
 */
class MBean implements Serializable {
	private static final long serialVersionUID = 1L;

	private final String name;

	private final String description;

	private final List<MBeanAttribute> attributes;

	static class MBeanAttribute implements Serializable {
		private static final long serialVersionUID = 1L;

		private final String name;

		private final String description;

		private final String formattedValue;

		MBeanAttribute(String name, String description, String formattedValue) {
			super();
			this.name = name;
			this.description = description;
			this.formattedValue = formattedValue;
		}

		String getName() {
			return name;
		}

		String getDescription() {
			return description;
		}

		String getFormattedValue() {
			return formattedValue;
		}
	}

	MBean(String name, String description, List<MBeanAttribute> attributes) {
		super();
		this.name = name;
		this.description = description;
		this.attributes = attributes;
	}

	String getName() {
		return name;
	}

	String getDescription() {
		return description;
	}

	List<MBeanAttribute> getAttributes() {
		return Collections.unmodifiableList(attributes);
	}
}

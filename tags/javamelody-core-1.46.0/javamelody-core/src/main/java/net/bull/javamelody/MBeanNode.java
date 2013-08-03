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
import java.util.ArrayList;
import java.util.List;

/**
 * Instance de données sérialisables représentant un noeud de MBean et construite à partir d'un ObjectName.
 * @author Emeric Vernat
 */
class MBeanNode implements Serializable {
	private static final long serialVersionUID = 1L;

	private final String name;

	private final String description;

	private final List<MBeanNode> children;

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

		/** {@inheritDoc} */
		@Override
		public String toString() {
			return getClass().getSimpleName() + "[name=" + getName() + ", formattedValue="
					+ getFormattedValue() + ']';
		}
	}

	MBeanNode(String name) {
		super();
		this.name = name;
		this.description = null;
		this.children = new ArrayList<MBeanNode>();
		this.attributes = null;
	}

	MBeanNode(String name, String description, List<MBeanAttribute> attributes) {
		super();
		this.name = name;
		this.description = description;
		this.children = null;
		this.attributes = attributes;
	}

	String getName() {
		return name;
	}

	String getDescription() {
		return description;
	}

	List<MBeanNode> getChildren() {
		return children != null ? children : null;
	}

	List<MBeanAttribute> getAttributes() {
		return attributes != null ? attributes : null;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ']';
	}
}

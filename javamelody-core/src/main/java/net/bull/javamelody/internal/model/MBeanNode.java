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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Instance de données sérialisables représentant un noeud de MBean et construite à partir d'un ObjectName.
 * @author Emeric Vernat
 */
public class MBeanNode implements Serializable {
	private static final long serialVersionUID = 1L;

	private final String name;

	private final String description;

	private final List<MBeanNode> children;

	private final List<MBeanAttribute> attributes;

	/**
	 * Attribut d'un MBean.
	 * @author Emeric Vernat
	 */
	public static class MBeanAttribute implements Serializable {
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

		public String getName() {
			return name;
		}

		public String getDescription() {
			return description;
		}

		public String getFormattedValue() {
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

	public String getName() {
		return name;
	}

	public String getDescription() {
		return description;
	}

	public List<MBeanNode> getChildren() {
		return children;
	}

	public List<MBeanAttribute> getAttributes() {
		return attributes;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ']';
	}
}

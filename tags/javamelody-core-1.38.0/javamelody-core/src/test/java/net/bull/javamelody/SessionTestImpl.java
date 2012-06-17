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

import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

/**
 * Implementation test de HttpSession.
 * @author Emeric Vernat
 */
public class SessionTestImpl implements HttpSession {
	private final Map<String, Object> attributes;
	private boolean invalidated; // false par défaut
	private String id = "id session";
	private long lastAccess = System.currentTimeMillis() - 3;

	SessionTestImpl(boolean serializable) {
		super();
		attributes = new LinkedHashMap<String, Object>();
		if (serializable) {
			attributes.put(SessionInformations.SESSION_COUNTRY_KEY, "fr");
			attributes.put(SessionInformations.SESSION_REMOTE_ADDR, "localhost");
			attributes.put(SessionInformations.SESSION_REMOTE_USER, "admin");
			attributes.put("test", null);
		} else {
			attributes.put("not serializable", new Object());
			final Object exceptionInToString = new Object() {
				/** {@inheritDoc} */
				@Override
				public String toString() {
					throw new IllegalStateException("il y a une erreur");
				}
			};
			attributes.put("exception in toString()", exceptionInToString);
		}
	}

	SessionTestImpl(String id, boolean serializable, long lastAccess) {
		this(serializable);
		this.id = id;
		this.lastAccess = lastAccess;
	}

	void setCountry(String country) {
		attributes.put(SessionInformations.SESSION_COUNTRY_KEY, country);
	}

	boolean isInvalidated() {
		return invalidated;
	}

	/** {@inheritDoc} */
	@Override
	public Object getAttribute(String name) {
		return attributes.get(name);
	}

	/** {@inheritDoc} */
	@Override
	public Enumeration<String> getAttributeNames() {
		return Collections.enumeration(attributes.keySet());
	}

	/** {@inheritDoc} */
	@Override
	public long getCreationTime() {
		return System.currentTimeMillis() - 300000;
	}

	/** {@inheritDoc} */
	@Override
	public String getId() {
		return id;
	}

	/** {@inheritDoc} */
	@Override
	public long getLastAccessedTime() {
		return lastAccess;
	}

	/** {@inheritDoc} */
	@Override
	public int getMaxInactiveInterval() {
		return 20 * 60;
	}

	/** {@inheritDoc} */
	@Override
	public ServletContext getServletContext() {
		return null;
	}

	/** @deprecated déprécié
	 *  @return HttpSessionContext */
	@Override
	@Deprecated
	public javax.servlet.http.HttpSessionContext getSessionContext() {
		return null;
	}

	/** @deprecated déprécié
	 * @param name String
	 * @return Object */
	@Override
	@Deprecated
	public Object getValue(String name) {
		return null;
	}

	/** @deprecated déprécié
	 * @return String[] */
	@Override
	@Deprecated
	public String[] getValueNames() {
		return null;
	}

	/** {@inheritDoc} */
	@Override
	public void invalidate() {
		invalidated = true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isNew() {
		return false;
	}

	/** @deprecated déprécié
	 * @param name String
	 * @param value Object */
	@Override
	@Deprecated
	public void putValue(String name, Object value) {
		// rien
	}

	/** {@inheritDoc} */
	@Override
	public void removeAttribute(String name) {
		// rien
	}

	/** @deprecated déprécié
	 * @param name String */
	@Override
	@Deprecated
	public void removeValue(String name) {
		// rien
	}

	/** {@inheritDoc} */
	@Override
	public void setAttribute(String name, Object value) {
		attributes.put(name, value);
	}

	/** {@inheritDoc} */
	@Override
	public void setMaxInactiveInterval(int interval) {
		// rien
	}
}

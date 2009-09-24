/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import net.bull.javamelody.SessionInformations;

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
	public Object getAttribute(String name) {
		return attributes.get(name);
	}

	/** {@inheritDoc} */
	public Enumeration<?> getAttributeNames() {
		return Collections.enumeration(attributes.keySet());
	}

	/** {@inheritDoc} */
	public long getCreationTime() {
		return System.currentTimeMillis() - 300000;
	}

	/** {@inheritDoc} */
	public String getId() {
		return id;
	}

	/** {@inheritDoc} */
	public long getLastAccessedTime() {
		return lastAccess;
	}

	/** {@inheritDoc} */
	public int getMaxInactiveInterval() {
		return 20 * 60;
	}

	/** {@inheritDoc} */
	public ServletContext getServletContext() {
		return null;
	}

	/** @deprecated déprécié
	 *  @return HttpSessionContext */
	@Deprecated
	public javax.servlet.http.HttpSessionContext getSessionContext() {
		return null;
	}

	/** {@inheritDoc} */
	public Object getValue(String name) {
		return null;
	}

	/** {@inheritDoc} */
	public String[] getValueNames() {
		return null;
	}

	/** {@inheritDoc} */
	public void invalidate() {
		invalidated = true;
	}

	/** {@inheritDoc} */
	public boolean isNew() {
		return false;
	}

	/** {@inheritDoc} */
	public void putValue(String name, Object value) {
		// rien
	}

	/** {@inheritDoc} */
	public void removeAttribute(String name) {
		// rien
	}

	/** {@inheritDoc} */
	public void removeValue(String name) {
		// rien
	}

	/** {@inheritDoc} */
	public void setAttribute(String name, Object value) {
		// rien
	}

	/** {@inheritDoc} */
	public void setMaxInactiveInterval(int interval) {
		// rien
	}
}

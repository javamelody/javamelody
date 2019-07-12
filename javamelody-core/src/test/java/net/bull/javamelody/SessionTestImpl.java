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

	public SessionTestImpl(boolean serializable) {
		super();
		attributes = new LinkedHashMap<String, Object>();
		if (serializable) {
			attributes.put(SessionListener.SESSION_COUNTRY_KEY, "fr");
			attributes.put(SessionListener.SESSION_REMOTE_ADDR, "localhost");
			attributes.put(SessionListener.SESSION_REMOTE_USER, "admin");
			attributes.put(SessionListener.SESSION_USER_AGENT,
					"Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko");
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

	public SessionTestImpl(String id, boolean serializable, long lastAccess) {
		this(serializable);
		this.id = id;
		this.lastAccess = lastAccess;
	}

	public void setCountry(String country) {
		attributes.put(SessionListener.SESSION_COUNTRY_KEY, country);
	}

	public void setUserAgent(String userAgent) {
		attributes.put(SessionListener.SESSION_USER_AGENT, userAgent);
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

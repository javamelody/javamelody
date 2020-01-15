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

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpSession;

import net.bull.javamelody.SessionListener;
import net.bull.javamelody.internal.common.I18N;

/**
 * Informations sur une session http.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une session http à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
public class SessionInformations implements Serializable {
	private static final long serialVersionUID = -2689338895804445093L;

	private static final List<String> BROWSERS = Arrays.asList("Edge", "Edg", "Chrome", "CriOS",
			"Firefox", "Safari", "MSIE", "Trident", "Opera" // IEMobile dans MSIE
	);

	private static final List<String> OS = Arrays.asList(
			// Android avant Linux. iPhone, iPad dans Mac OS. *bot et (yahoo) slurp ignorés
			"Windows", "Android", "Linux", "Mac OS");

	private static final Map<String, String> WINDOWS_CODE_TO_NAME_MAP = new LinkedHashMap<String, String>();

	static {
		// see https://msdn.microsoft.com/en-us/library/ms537503%28v=vs.85%29.aspx
		WINDOWS_CODE_TO_NAME_MAP.put("NT 6.3", "8.1");
		WINDOWS_CODE_TO_NAME_MAP.put("NT 6.2", "8");
		WINDOWS_CODE_TO_NAME_MAP.put("NT 6.1", "7");
		WINDOWS_CODE_TO_NAME_MAP.put("NT 6.0", "Vista");
		WINDOWS_CODE_TO_NAME_MAP.put("NT 5.2", "Server 2003/XP");
		WINDOWS_CODE_TO_NAME_MAP.put("NT 5.1", "XP");
		// others ommitted
	}

	// on utilise ce ByteArrayOutputStream pour calculer les tailles sérialisées,
	// on n'a qu'une instance pour éviter d'instancier un gros tableau d'octets à chaque session
	@SuppressWarnings("all")
	private static final ByteArrayOutputStream TEMP_OUTPUT = new ByteArrayOutputStream(8 * 1024);
	private final String id;
	private final Date lastAccess;
	private final Date age;
	private final Date expirationDate;
	private final int attributeCount;
	private final boolean serializable;
	private final String country;
	private final String remoteAddr;
	private final String remoteUser;
	private final String userAgent;
	private final int serializedSize;
	@SuppressWarnings("all")
	private final List<SessionAttribute> attributes;

	/**
	 * Attribut de session.
	 * @author Emeric Vernat
	 */
	public static class SessionAttribute implements Serializable {
		private static final long serialVersionUID = 4786854834871331127L;
		private final String name;
		private final String type;
		private final String content;
		private final boolean serializable;
		private final int serializedSize;

		SessionAttribute(HttpSession session, String attributeName) {
			super();
			assert session != null;
			assert attributeName != null;
			name = attributeName;
			final Object value = session.getAttribute(attributeName);
			serializable = value == null || value instanceof Serializable;
			serializedSize = getObjectSize(value);
			if (value == null) {
				content = null;
				type = null;
			} else {
				String tmp;
				try {
					tmp = String.valueOf(value);
				} catch (final Exception e) {
					tmp = e.toString();
				}
				content = tmp;
				type = value.getClass().getName();
			}
		}

		public String getName() {
			return name;
		}

		public String getType() {
			return type;
		}

		public String getContent() {
			return content;
		}

		public boolean isSerializable() {
			return serializable;
		}

		public int getSerializedSize() {
			return serializedSize;
		}
	}

	public SessionInformations(HttpSession session, boolean includeAttributes) {
		super();
		assert session != null;
		id = session.getId();
		final long now = System.currentTimeMillis();
		lastAccess = new Date(now - session.getLastAccessedTime());
		age = new Date(now - session.getCreationTime());
		expirationDate = new Date(
				session.getLastAccessedTime() + session.getMaxInactiveInterval() * 1000L);

		final List<String> attributeNames = Collections.list(session.getAttributeNames());
		attributeCount = attributeNames.size();
		serializable = computeSerializable(session, attributeNames);

		final Object countryCode = session.getAttribute(SessionListener.SESSION_COUNTRY_KEY);
		if (countryCode == null) {
			country = null;
		} else {
			country = countryCode.toString().toLowerCase(Locale.ENGLISH);
		}

		final Object addr = session.getAttribute(SessionListener.SESSION_REMOTE_ADDR);
		if (addr == null) {
			remoteAddr = null;
		} else {
			remoteAddr = addr.toString();
		}

		Object user = session.getAttribute(SessionListener.SESSION_REMOTE_USER);
		if (user == null) {
			// si getRemoteUser() n'était pas renseigné, on essaye ACEGI_SECURITY_LAST_USERNAME
			// (notamment pour Jenkins)
			user = session.getAttribute("ACEGI_SECURITY_LAST_USERNAME");
			if (user == null) {
				// et sinon SPRING_SECURITY_LAST_USERNAME
				user = session.getAttribute("SPRING_SECURITY_LAST_USERNAME");
			}
		}
		if (user == null) {
			remoteUser = null;
		} else {
			remoteUser = user.toString();
		}

		final Object agent = session.getAttribute(SessionListener.SESSION_USER_AGENT);
		if (agent == null) {
			userAgent = null;
		} else {
			userAgent = agent.toString();
		}

		serializedSize = computeSerializedSize(session, attributeNames);

		if (includeAttributes) {
			attributes = new ArrayList<SessionAttribute>(attributeCount);
			for (final String attributeName : attributeNames) {
				attributes.add(new SessionAttribute(session, attributeName));
			}
		} else {
			attributes = null;
		}
	}

	private boolean computeSerializable(HttpSession session, List<String> attributeNames) {
		for (final String attributeName : attributeNames) {
			final Object attributeValue = session.getAttribute(attributeName);
			if (!(attributeValue == null || attributeValue instanceof Serializable)) {
				return false;
			}
		}
		return true;
	}

	private int computeSerializedSize(HttpSession session, List<String> attributeNames) {
		if (!serializable) {
			// la taille pour la session est inconnue si un de ses attributs n'est pas sérialisable
			return -1;
		}
		// On calcule la taille sérialisée de tous les attributs en sérialisant une liste les contenant

		// Rq : la taille sérialisée des attributs ensembles peut être très inférieure à la somme
		// des tailles sérialisées, car des attributs peuvent référencer des objets communs,
		// mais la liste contenant introduit un overhead fixe sur le résultat par rapport à la somme des tailles
		// (de même que l'introduirait la sérialisation de l'objet HttpSession avec ses propriétés standards)

		// Rq : on ne peut calculer la taille sérialisée exacte de l'objet HttpSession
		// car cette interface JavaEE n'est pas déclarée Serializable et l'implémentation au moins dans tomcat
		// n'est pas Serializable non plus

		// Rq : la taille occupée en mémoire par une session http est un peu différente de la taille sérialisée
		// car la sérialisation d'un objet est différente des octets occupés en mémoire
		// et car les objets retenus par une session sont éventuellement référencés par ailleurs
		// (la fin de la session ne réduirait alors pas l'occupation mémoire autant que la taille de la session)
		final List<Serializable> serializableAttributes = new ArrayList<Serializable>(
				attributeNames.size());
		for (final String attributeName : attributeNames) {
			final Object attributeValue = session.getAttribute(attributeName);
			serializableAttributes.add((Serializable) attributeValue);
		}
		return getObjectSize(serializableAttributes);
	}

	public String getId() {
		return id;
	}

	public Date getLastAccess() {
		return lastAccess;
	}

	public Date getAge() {
		return age;
	}

	public Date getExpirationDate() {
		return expirationDate;
	}

	public int getAttributeCount() {
		return attributeCount;
	}

	public boolean isSerializable() {
		return serializable;
	}

	public String getCountry() {
		return country;
	}

	public String getCountryDisplay() {
		final String myCountry = getCountry();
		if (myCountry == null) {
			return null;
		}
		// "fr" est sans conséquence
		return new Locale("fr", myCountry).getDisplayCountry(I18N.getCurrentLocale());
	}

	public String getRemoteAddr() {
		return remoteAddr;
	}

	public String getBrowser() {
		if (userAgent == null) {
			return null;
		}
		final String[] userAgentSplitted = userAgent.split("[ ;]");
		for (final String browser : BROWSERS) {
			for (final String ua : userAgentSplitted) {
				if (ua.contains(browser)) {
					final String result = ua.trim();
					// for user agent: Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; AS; rv:11.0) like Gecko
					// see http://www.useragentstring.com/pages/Internet%20Explorer/
					return result.replace("Trident/7.0", "MSIE 11.0");
				}
			}
		}
		// browser unknown, the complete userAgent will still be displayed in the session attributes
		return null;
	}

	public String getOs() {
		if (userAgent == null) {
			return null;
		}
		final String[] userAgentSplitted = userAgent.split("[();]");
		for (final String os : OS) {
			for (final String ua : userAgentSplitted) {
				if (ua.contains(os)) {
					String result = ua.trim();
					if (result.contains("Windows")) {
						for (final Map.Entry<String, String> entry : WINDOWS_CODE_TO_NAME_MAP
								.entrySet()) {
							final String code = entry.getKey();
							final String name = entry.getValue();
							result = result.replace(code, name);
						}
					}
					return result;
				}
			}
		}
		// OS unknown, the complete userAgent will still be displayed in the session attributes
		return null;
	}

	public String getRemoteUser() {
		return remoteUser;
	}

	public int getSerializedSize() {
		return serializedSize;
	}

	public List<SessionAttribute> getAttributes() {
		return Collections.unmodifiableList(attributes);
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[id=" + getId() + ", remoteAddr=" + getRemoteAddr()
				+ ", serializedSize=" + getSerializedSize() + ']';
	}

	static int getObjectSize(Object object) {
		if (!(object instanceof Serializable)) {
			return -1;
		}
		final Serializable serializable = (Serializable) object;
		// synchronized pour protéger l'accès à TEMP_OUTPUT static
		synchronized (TEMP_OUTPUT) {
			TEMP_OUTPUT.reset();
			try {
				final ObjectOutputStream out = new ObjectOutputStream(TEMP_OUTPUT);
				try {
					out.writeObject(serializable);
				} finally {
					out.close();
				}
				return TEMP_OUTPUT.size();
			} catch (final Throwable e) { // NOPMD
				// ce catch Throwable inclut IOException et aussi NoClassDefFoundError/ClassNotFoundException (issue 355)
				return -1;
			}
		}
	}
}

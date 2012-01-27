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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpSession;

/**
 * Informations sur une session http.
 * L'état d'une instance est initialisé à son instanciation et non mutable;
 * il est donc de fait thread-safe.
 * Cet état est celui d'une session http à un instant t.
 * Les instances sont sérialisables pour pouvoir être transmises au serveur de collecte.
 * @author Emeric Vernat
 */
class SessionInformations implements Serializable {
	static final String SESSION_COUNTRY_KEY = "javamelody.country";
	static final String SESSION_REMOTE_ADDR = "javamelody.remoteAddr";
	static final String SESSION_REMOTE_USER = "javamelody.remoteUser";
	private static final long serialVersionUID = -2689338895804445093L;
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
	private final int serializedSize;
	@SuppressWarnings("all")
	private final List<SessionAttribute> attributes;

	static class SessionAttribute implements Serializable {
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

		String getName() {
			return name;
		}

		String getType() {
			return type;
		}

		String getContent() {
			return content;
		}

		boolean isSerializable() {
			return serializable;
		}

		int getSerializedSize() {
			return serializedSize;
		}
	}

	@SuppressWarnings("unchecked")
	SessionInformations(HttpSession session, boolean includeAttributes) {
		super();
		assert session != null;
		id = session.getId();
		final long now = System.currentTimeMillis();
		lastAccess = new Date(now - session.getLastAccessedTime());
		age = new Date(now - session.getCreationTime());
		expirationDate = new Date(session.getLastAccessedTime() + session.getMaxInactiveInterval()
				* 1000L);

		final List<String> attributeNames = Collections.list(session.getAttributeNames());
		attributeCount = attributeNames.size();
		serializable = computeSerializable(session, attributeNames);

		final Object countryCode = session.getAttribute(SESSION_COUNTRY_KEY);
		if (countryCode == null) {
			country = null;
		} else {
			country = countryCode.toString().toLowerCase(Locale.getDefault());
		}

		final Object addr = session.getAttribute(SESSION_REMOTE_ADDR);
		if (addr == null) {
			remoteAddr = null;
		} else {
			remoteAddr = addr.toString();
		}

		Object user = session.getAttribute(SESSION_REMOTE_USER);
		if (user == null) {
			// si getRemoteUser() n'était pas renseigné, on essaye ACEGI_SECURITY_LAST_USERNAME
			// (notamment pour Hudson/Jenkins)
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

	String getId() {
		return id;
	}

	Date getLastAccess() {
		return lastAccess;
	}

	Date getAge() {
		return age;
	}

	Date getExpirationDate() {
		return expirationDate;
	}

	int getAttributeCount() {
		return attributeCount;
	}

	boolean isSerializable() {
		return serializable;
	}

	String getCountry() {
		return country;
	}

	String getCountryDisplay() {
		final String myCountry = getCountry();
		if (myCountry == null) {
			return null;
		}
		// "fr" est sans conséquence
		return new Locale("fr", myCountry).getDisplayCountry(I18N.getCurrentLocale());
	}

	String getRemoteAddr() {
		return remoteAddr;
	}

	String getRemoteUser() {
		return remoteUser;
	}

	int getSerializedSize() {
		return serializedSize;
	}

	List<SessionAttribute> getAttributes() {
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
			} catch (final IOException e) {
				return -1;
			}
		}
	}
}

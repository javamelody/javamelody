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
package net.bull.javamelody.internal.common;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Date;
import java.util.List;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.naming.InitialContext;
import javax.naming.NameNotFoundException;
import javax.naming.NamingException;

/**
 * Cette classe permet d'envoyer des emails.
 * @author Emeric Vernat
 */
public class Mailer {
	private final String jndiName;
	private Session session;
	private InternetAddress fromAddress;

	/**
	 * Constructeur.
	 * @param jndiName String
	 */
	public Mailer(String jndiName) {
		super();
		assert jndiName != null;
		this.jndiName = jndiName;
	}

	/**
	 * Instanciation à la demande car cela peut être long en particulier si le serveur de mail est loin.
	 * @return Session
	 * @throws NamingException e
	 */
	private Session getSession() throws NamingException {
		if (session == null) {
			synchronized (this) {
				try {
					session = (Session) lookupFromJndiName();
				} catch (final ClassCastException e) {
					// la déclaration d'une session mail dans un contexte tomcat par exemple
					// nécessite d'avoir les jars javamail et activation dans le répertoire lib
					// du serveur, mais si ces 2 jars sont aussi dans WEB-INF/lib de la webapp
					// pour une utilisation directe sans session mail alors on obtient ClassCastException
					// de javax.mail.Session en javax.mail.Session.
					// Pour pallier cela, on contourne ClassCastException en récupérant
					// les propriétés dans la session mail du serveur sans faire de cast
					// puis on recrée une session mail locale avec les jars de la webapp.
					session = Session.getInstance(getPropertiesFromSession(lookupFromJndiName()));
				}
			}
			fromAddress = InternetAddress.getLocalAddress(session);
		}
		return session;
	}

	private Object lookupFromJndiName() throws NamingException {
		final InitialContext ctx = new InitialContext();
		try {
			return ctx.lookup("java:comp/env/" + jndiName);
		} catch (final NameNotFoundException e) {
			try {
				return ctx.lookup("java:/" + jndiName);
			} catch (final NameNotFoundException e2) {
				return ctx.lookup(jndiName);
			}
		} finally {
			ctx.close();
		}
	}

	// pour tests unitaires
	void setSession(Session session) {
		this.session = session;
		fromAddress = InternetAddress.getLocalAddress(session);
	}

	private Properties getPropertiesFromSession(Object jndiSession) {
		// On récupère ici les propriétés déclarées d'une session mail par réflexion.
		// Le paramètre jndiSession n'est pas indiqué de type Session car le cast ne marcherait pas.
		final Method getPropertiesMethod;
		try {
			getPropertiesMethod = jndiSession.getClass().getMethod("getProperties", (Class[]) null);
			return (Properties) getPropertiesMethod.invoke(jndiSession, (Object[]) null);
		} catch (final NoSuchMethodException e) {
			throw new IllegalArgumentException(e);
		} catch (final InvocationTargetException e) {
			throw new IllegalStateException(e.getCause());
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Envoie un mail.
	 * @param toAddress Adresses mails des destinataires séparées par des virgules.
	 * @param subject Titre du mail
	 * @param message Corps du mail
	 * @param attachments Liste de fichiers à attacher
	 * @param highPriority Priorité haute
	 * @throws NamingException e
	 * @throws MessagingException e
	 */
	public void send(String toAddress, String subject, String message, List<File> attachments,
			boolean highPriority) throws NamingException, MessagingException {
		assert toAddress != null;
		assert subject != null;
		assert message != null;
		final InternetAddress[] toAddresses = InternetAddress.parse(toAddress, false);
		final Message msg = new MimeMessage(getSession());
		msg.setRecipients(Message.RecipientType.TO, toAddresses);
		msg.setSubject(subject);
		msg.setSentDate(new Date());
		msg.setFrom(fromAddress);
		if (highPriority) {
			msg.setHeader("X-Priority", "1");
			msg.setHeader("x-msmail-priority", "high");
		}

		// Content is stored in a MIME multi-part message with one body part
		final MimeBodyPart mbp = new MimeBodyPart();
		mbp.setText(message);
		final Multipart multipart = new MimeMultipart();
		multipart.addBodyPart(mbp);
		if (attachments != null && !attachments.isEmpty()) {
			for (final File attachment : attachments) {
				final DataSource source = new FileDataSource(attachment);
				final MimeBodyPart messageBodyPart = new MimeBodyPart();
				messageBodyPart.setDataHandler(new DataHandler(source));
				messageBodyPart.setFileName(attachment.getName());
				multipart.addBodyPart(messageBodyPart);
			}
		}
		msg.setContent(multipart);
		// protocol smtp, ou smpts si mail.transport.protocol=smtps est indiqué dans la configuration de la session mail
		String protocol = session.getProperty("mail.transport.protocol");
		if (protocol == null) {
			protocol = "smtp";
		}
		// authentification avec user et password si mail.smtp.auth=true (ou mail.smtps.auth=true)
		if (Boolean.parseBoolean(session.getProperty("mail." + protocol + ".auth"))) {
			final Transport tr = session.getTransport(protocol);
			try {
				tr.connect(session.getProperty("mail." + protocol + ".user"),
						session.getProperty("mail." + protocol + ".password"));
				msg.saveChanges(); // don't forget this
				tr.sendMessage(msg, msg.getAllRecipients());
			} finally {
				tr.close();
			}
		} else {
			Transport.send(msg);
		}
	}
}

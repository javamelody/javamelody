/*
 * Copyright 2008-2010 by Emeric Vernat
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
import javax.naming.NamingException;

/**
 * Cette classe permet d'envoyer des emails.
 * @author Emeric Vernat
 */
class Mailer {
	private final String jndiName;
	private Session session;
	private InternetAddress fromAddress;

	/**
	 * Constructeur.
	 * @param jndiName String
	 */
	Mailer(String jndiName) {
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
				final InitialContext ctx = new InitialContext();
				try {
					session = (Session) ctx.lookup(jndiName);
				} catch (final ClassCastException e) {
					// la déclaration d'une session mail dans un contexte tomcat par exemple
					// nécessite d'avoir les jars javamail et activation dans le répertoire lib
					// du serveur, mais si ces 2 jars sont aussi dans WEB-INF/lib de la webapp
					// pour une utilisation directe sans session mail alors on obtient ClassCastException
					// de javax.mail.Session en javax.mail.Session.
					// Pour pallier cela, on contourne ClassCastException en récupérant
					// les propriétés dans la session mail du serveur sans faire de cast
					// puis on recrée une session mail locale avec les jars de la webapp.
					session = Session.getInstance(getPropertiesFromSession(ctx.lookup(jndiName)));
				}
				ctx.close();
			}
			fromAddress = InternetAddress.getLocalAddress(session);
		}
		return session;
	}

	private Properties getPropertiesFromSession(Object jndiSession) {
		// On récupère ici les propriétés déclarées d'une session mail par réflexion.
		// Le paramètre jndiSession n'est pas indiqué de type Session car le cast ne marcherait pas.
		Method getPropertiesMethod;
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
	void send(String toAddress, String subject, String message, List<File> attachments,
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
				tr.connect(session.getProperty("mail." + protocol + ".user"), session
						.getProperty("mail." + protocol + ".password"));
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

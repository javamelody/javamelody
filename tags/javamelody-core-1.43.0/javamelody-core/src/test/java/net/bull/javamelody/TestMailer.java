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

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.naming.NamingException;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe Mailer.
 * @author Emeric Vernat
 */
public class TestMailer {
	private Mailer mailer;

	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
		mailer = new Mailer("jndi name");
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testMailerSend() throws Exception { // NOPMD
		try {
			send(null, false);
		} catch (final NamingException e) {
			assertNotNull("ok", e);
		}
		final Properties properties = new Properties();
		mailer.setSession(Session.getInstance(properties));
		send(null, false);
		send(null, true);
		send(new ArrayList<File>(), false);
		send(Collections.singletonList(new File("nothing.txt")), false);
		properties.put("mail.transport.protocol", "smtps");
		mailer.setSession(Session.getInstance(properties));
		send(null, false);
		properties.put("mail.smtps.auth", "true");
		mailer.setSession(Session.getInstance(properties));
		send(null, false);
	}

	private void send(List<File> attachments, boolean highPriority) throws NamingException {
		final String toAddress = "test";
		final String subject = "test subject";
		final String message = "test message";
		try {
			mailer.send(toAddress, subject, message, attachments, highPriority);
		} catch (final MessagingException e) {
			assertNotNull("ok", e);
		}
	}
}

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

import net.bull.javamelody.Utils;

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
	public void testMailerSend() throws Exception {
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

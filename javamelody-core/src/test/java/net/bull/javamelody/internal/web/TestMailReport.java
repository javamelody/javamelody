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
package net.bull.javamelody.internal.web;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Collections;
import java.util.List;
import java.util.Timer;

import javax.naming.NoInitialContextException;
import javax.servlet.ServletContext;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Parameter;
import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.Collector;
import net.bull.javamelody.internal.model.Counter;
import net.bull.javamelody.internal.model.JavaInformations;
import net.bull.javamelody.internal.model.Period;

/**
 * Test unitaire de la classe MailReport.
 * @author Emeric Vernat
 */
public class TestMailReport {
	/** Check. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	public void testScheduleReportMail() {
		final Timer timer = new Timer("test timer", true);
		try {
			final Counter counter = new Counter("http", null);
			final Collector collector = new Collector("test", Collections.singletonList(counter));
			MailReport.scheduleReportMailForLocalServer(collector, timer);
		} finally {
			timer.cancel();
		}
		// n'importe
		assertNotNull("MailReport", timer.purge());
	}

	/** Test.
	 * @throws Exception e */
	@Test
	public void testSendReportMail() throws Exception {
		final Counter counter = new Counter("http", null);
		final Collector collector = new Collector("test", Collections.singletonList(counter));
		final List<JavaInformations> javaInformationslist = Collections
				.singletonList(new JavaInformations(null, true));
		setProperty(Parameter.ADMIN_EMAILS, "evernat@free.fr");
		setProperty(Parameter.MAIL_SESSION, "mail/Session");
		try {
			new MailReport().sendReportMail(collector, false, javaInformationslist, Period.SEMAINE);
		} catch (final NoInitialContextException e) {
			assertNotNull("ok", e);
		}
		setProperty(Parameter.MAIL_SUBJECT_PREFIX, "[javamelody] ");
		try {
			new MailReport().sendReportMail(collector, false, javaInformationslist, Period.SEMAINE);
		} catch (final NoInitialContextException e) {
			assertNotNull("ok", e);
		}

		// sendReportMailForLocalServer
		final String path = "path";
		final ServletContext context = createNiceMock(ServletContext.class);
		expect(context.getMajorVersion()).andReturn(3).anyTimes();
		expect(context.getMinorVersion()).andReturn(0).anyTimes();
		expect(context.getContextPath()).andReturn(path).anyTimes();
		replay(context);
		Parameters.initialize(context);
		try {
			new MailReport().sendReportMailForLocalServer(collector, Period.SEMAINE);
		} catch (final NoInitialContextException e) {
			assertNotNull("ok", e);
		}
		verify(context);
	}

	/** Test. */
	@Test
	public void testGetNextExecutionDate() {
		assertNotNull("getNextExecutionDate", MailReport.getNextExecutionDate(Period.JOUR));
		assertNotNull("getNextExecutionDate", MailReport.getNextExecutionDate(Period.SEMAINE));
		assertNotNull("getNextExecutionDate", MailReport.getNextExecutionDate(Period.MOIS));
	}

	/** Test. */
	@Test
	public void testGetMailPeriod() {
		for (final Period period : Period.values()) {
			assertNotNull("getMailCode", period.getMailCode());
			assertEquals("valueOfByMailCode", period,
					Period.valueOfByMailCode(period.getMailCode()));
		}
		try {
			Period.valueOfByMailCode("unknown");
		} catch (final IllegalArgumentException e) {
			assertNotNull("ok", e);
		}

		assertEquals("getMailPeriods", Collections.singletonList(Period.SEMAINE),
				MailReport.getMailPeriods());
		setProperty(Parameter.MAIL_PERIODS, Period.JOUR.getMailCode());
		assertEquals("getMailPeriods", Collections.singletonList(Period.JOUR),
				MailReport.getMailPeriods());
	}

	private static void setProperty(Parameter parameter, String value) {
		Utils.setProperty(parameter, value);
	}
}

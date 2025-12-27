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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;
import net.bull.javamelody.internal.common.I18N;

/**
 * Test unitaire de la classe Range.
 * @author Emeric Vernat
 */
class TestRange {
	private static final long ONE_DAY_SECONDS = 24 * 60 * 60;
	private static final long ONE_DAY_MILLIS = ONE_DAY_SECONDS * 1000;
	private static final long ONE_MINUTE_MILLIS = 60 * 1000;
	private Range periodRange;
	private Range customRange;

	/** Test. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
		periodRange = Period.JOUR.getRange();
		customRange = Range.createCustomRange(new Date(System.currentTimeMillis() - ONE_DAY_MILLIS),
				new Date());
	}

	/** Test. */
	@Test
	void testGetPeriod() {
		assertNotNull(periodRange.getPeriod(), "getPeriod");
		assertNull(customRange.getPeriod(), "getPeriod");
	}

	/** Test. */
	@Test
	void testGetStartDate() {
		assertNull(periodRange.getStartDate(), "getStartDate");
		assertNotNull(customRange.getStartDate(), "getStartDate");
	}

	/** Test. */
	@Test
	void testGetEndDate() {
		assertNull(periodRange.getEndDate(), "getEndDate");
		assertNotNull(customRange.getEndDate(), "getEndDate");
	}

	/** Test. */
	@Test
	void testGetValue() {
		assertNotNull(periodRange.getValue(), "getValue");
		assertNotNull(customRange.getValue(), "getValue");
	}

	/** Test. */
	@Test
	void testParse() {
		I18N.bindLocale(Locale.FRENCH);
		try {
			final DateFormat dateFormat = I18N.createDateFormat();
			assertEquals(periodRange.getPeriod(),
					Range.parse(periodRange.getValue(), dateFormat).getPeriod(),
					"parse1");
			assertTrue(isSameDay(customRange.getStartDate(),
					Range.parse(customRange.getValue(), dateFormat).getStartDate()),
					"parse2");
			assertTrue(isSameDay(customRange.getEndDate(),
					Range.parse(customRange.getValue(), dateFormat).getEndDate()),
					"parse3");

			// on teste le résultat en cas d'erreur de format
			assertNotNull(Range
					.parse("xxxxxx" + Range.CUSTOM_PERIOD_SEPARATOR + "01/01/2010", dateFormat),
					"parse4");
			assertNotNull(Range
					.parse("01/01/2010" + Range.CUSTOM_PERIOD_SEPARATOR + "xxxxxx", dateFormat),
					"parse5");
			assertNotNull(Range.parse("01/01/2010" + Range.CUSTOM_PERIOD_SEPARATOR, dateFormat), "parse6");
			assertNotNull(Range.parse("01/01/2011", dateFormat), "parse6b");
			// on teste les bornes min et max
			final Calendar calendar = Calendar.getInstance();
			final int currentYear = calendar.get(Calendar.YEAR);
			Range range = Range.parse("01/01/2000" + Range.CUSTOM_PERIOD_SEPARATOR + "01/01/2030",
					dateFormat);
			calendar.setTime(range.getStartDate());
			assertTrue(calendar.get(Calendar.YEAR) >= currentYear - 2, "parse7");
			calendar.setTime(range.getEndDate());
			assertTrue(calendar.get(Calendar.YEAR) <= currentYear, "parse7");
			range = Range.parse("01/01/2030" + Range.CUSTOM_PERIOD_SEPARATOR + "01/01/2030",
					dateFormat);
			calendar.setTime(range.getStartDate());
			assertTrue(calendar.get(Calendar.YEAR) <= currentYear, "parse8");
		} finally {
			I18N.unbindLocale();
		}
	}

	@SuppressWarnings("deprecation")
	private static boolean isSameDay(Date date1, Date date2) {
		return (date1.getTime() - date1.getTimezoneOffset() * ONE_MINUTE_MILLIS)
				/ ONE_DAY_MILLIS == (date2.getTime()
						- date2.getTimezoneOffset() * ONE_MINUTE_MILLIS) / ONE_DAY_MILLIS;
	}

	/** Test. */
	@Test
	void testGetLabel() {
		assertNotNull(periodRange.getLabel(), "getLabel");
		assertNotNull(customRange.getLabel(), "getLabel");
	}

	/** Test. */
	@Test
	void testGetDurationDays() {
		assertEquals(1L, periodRange.getDurationDays(), "getDurationDays");
		assertEquals(1L, customRange.getDurationDays(), "getDurationDays");
	}

	/** Test. */
	@Test
	void testToString() {
		final String string = periodRange.toString();
		assertNotNull(string, "toString not null");
		assertFalse(string.isEmpty(), "toString not empty");
		final String string2 = customRange.toString();
		assertNotNull(string2, "toString not null");
		assertFalse(string2.isEmpty(), "toString not empty");
	}

}

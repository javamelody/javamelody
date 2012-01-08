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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe Range.
 * @author Emeric Vernat
 */
public class TestRange {
	private static final long ONE_DAY_SECONDS = 24 * 60 * 60;
	private static final long ONE_DAY_MILLIS = ONE_DAY_SECONDS * 1000;
	private static final long ONE_MINUTE_MILLIS = 60 * 1000;
	private Range periodRange;
	private Range customRange;

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		periodRange = Period.JOUR.getRange();
		customRange = Range.createCustomRange(
				new Date(System.currentTimeMillis() - ONE_DAY_MILLIS), new Date());
	}

	/** Test. */
	@Test
	public void testGetPeriod() {
		assertNotNull("getPeriod", periodRange.getPeriod());
		assertNull("getPeriod", customRange.getPeriod());
	}

	/** Test. */
	@Test
	public void testGetStartDate() {
		assertNull("getStartDate", periodRange.getStartDate());
		assertNotNull("getStartDate", customRange.getStartDate());
	}

	/** Test. */
	@Test
	public void testGetEndDate() {
		assertNull("getEndDate", periodRange.getEndDate());
		assertNotNull("getEndDate", customRange.getEndDate());
	}

	/** Test. */
	@Test
	public void testGetValue() {
		assertNotNull("getValue", periodRange.getValue());
		assertNotNull("getValue", customRange.getValue());
	}

	/** Test. */
	@Test
	public void testParse() {
		I18N.bindLocale(Locale.FRENCH);
		try {
			assertEquals("parse1", periodRange.getPeriod(), Range.parse(periodRange.getValue())
					.getPeriod());
			assertTrue(
					"parse2",
					isSameDay(customRange.getStartDate(), Range.parse(customRange.getValue())
							.getStartDate()));
			assertTrue(
					"parse3",
					isSameDay(customRange.getEndDate(), Range.parse(customRange.getValue())
							.getEndDate()));

			// on teste le résultat en cas d'erreur de format
			assertNotNull("parse4", Range.parse("xxxxxx-01/01/2010"));
			assertNotNull("parse5", Range.parse("01/01/2010-xxxxxx"));
			assertNotNull("parse6", Range.parse("01/01/2010-"));
			// on teste les bornes min et max
			final Calendar calendar = Calendar.getInstance();
			final int currentYear = calendar.get(Calendar.YEAR);
			Range range = Range.parse("01/01/2000-01/01/2030");
			calendar.setTime(range.getStartDate());
			assertTrue("parse7", calendar.get(Calendar.YEAR) >= currentYear - 2);
			calendar.setTime(range.getEndDate());
			assertTrue("parse7", calendar.get(Calendar.YEAR) <= currentYear);
			range = Range.parse("01/01/2030-01/01/2030");
			calendar.setTime(range.getStartDate());
			assertTrue("parse8", calendar.get(Calendar.YEAR) <= currentYear);
		} finally {
			I18N.unbindLocale();
		}
	}

	@SuppressWarnings("deprecation")
	private static boolean isSameDay(Date date1, Date date2) {
		return (date1.getTime() - date1.getTimezoneOffset() * ONE_MINUTE_MILLIS) / ONE_DAY_MILLIS == (date2
				.getTime() - date2.getTimezoneOffset() * ONE_MINUTE_MILLIS) / ONE_DAY_MILLIS;
	}

	/** Test. */
	@Test
	public void testGetLabel() {
		assertNotNull("getLabel", periodRange.getLabel());
		assertNotNull("getLabel", customRange.getLabel());
	}

	/** Test. */
	@Test
	public void testGetDurationDays() {
		assertEquals("getDurationDays", 1L, periodRange.getDurationDays());
		assertEquals("getDurationDays", 1L, customRange.getDurationDays());
	}

	/** Test. */
	@Test
	public void testToString() {
		final String string = periodRange.toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
		final String string2 = customRange.toString();
		assertNotNull("toString not null", string2);
		assertFalse("toString not empty", string2.isEmpty());
	}

}

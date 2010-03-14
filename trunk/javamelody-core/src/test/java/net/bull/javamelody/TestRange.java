/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
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

import java.util.Date;

import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe Range.
 * @author Emeric Vernat
 */
public class TestRange {
	private static final long ONE_DAY_SECONDS = 24 * 60 * 60;
	private static final long ONE_DAY_MILLIS = ONE_DAY_SECONDS * 1000;
	private Range periodRange;
	private Range customRange;

	/** Test. */
	@Before
	public void setUp() {
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
		assertNotNull("parse", periodRange.getPeriod() == Range.parse(periodRange.getValue())
				.getPeriod());
		assertNotNull("parse", customRange.getStartDate().equals(
				Range.parse(customRange.getValue()).getStartDate()));
		assertNotNull("parse", customRange.getEndDate().equals(
				Range.parse(customRange.getValue()).getEndDate()));
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

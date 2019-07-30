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

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.Date;

import org.junit.Before;
import org.junit.Test;

import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe Range.
 * @author Emeric Vernat
 */
public class TestPeriodCounterFactory {
	private PeriodCounterFactory periodCounterFactory;

	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
		final Counter sqlCounter = new Counter("sql", "db.png");
		sqlCounter.setApplication("test");
		sqlCounter.addRequest("test", 1, 1, 1, false, -1);
		periodCounterFactory = new PeriodCounterFactory(sqlCounter);
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void test() throws IOException {
		assertNotNull("buildNewDayCounter", periodCounterFactory.buildNewDayCounter());
		assertNotNull("createDayCounterAtDate",
				periodCounterFactory.createDayCounterAtDate(new Date()));
		final Range customRange = Range.createCustomRange(
				new Date(System.currentTimeMillis() - 24L * 60 * 60 * 1000), new Date());
		assertNotNull("getCustomCounter", periodCounterFactory.getCustomCounter(customRange));
		assertNotNull("getDayCounter", periodCounterFactory.getDayCounter());
		assertNotNull("getMonthCounter", periodCounterFactory.getMonthCounter());
		assertNotNull("getWeekCounter", periodCounterFactory.getWeekCounter());
		assertNotNull("getYearCounter", periodCounterFactory.getYearCounter());
	}
}

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

import static org.junit.Assert.assertNotNull;

import org.junit.Test;

/**
 * Test unitaire de la classe MonitoringInterceptor.
 * @author Emeric Vernat
 */
public class TestMonitoringInterceptor {
	/** Test. */
	@Test
	public void testNewInstance() {
		assertNotNull("new MonitoringInterceptor", new MonitoringInterceptor());
	}

	/** Test. */
	@Test
	public void testGetEjbCounter() {
		assertNotNull("getEjbCounter", MonitoringInterceptor.getEjbCounter());
	}
}

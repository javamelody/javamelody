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
package net.bull.javamelody.internal.web.pdf;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;

/**
 * Test unitaire de la classe interne PdfJavaInformationsReport.Bar.
 * @author Emeric Vernat
 */
public class TestPdfBar {

	/** Before. */
	@BeforeEach
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testToPdf() throws IOException {
		assertNotNull(Bar.toBar(0), "toBar");
		assertNotNull(Bar.toBar(1), "toBar");
		assertNotNull(Bar.toBar(10), "toBar");
		assertNotNull(Bar.toBar(15), "toBar");
		assertNotNull(Bar.toBarWithAlert(10), "toBarWithAlert");
		assertNotNull(Bar.toBarWithAlert(100), "toBarWithAlert");
	}
}

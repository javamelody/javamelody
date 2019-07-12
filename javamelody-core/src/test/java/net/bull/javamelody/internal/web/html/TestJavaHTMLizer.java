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
package net.bull.javamelody.internal.web.html;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Test unitaire pour JavaHTMLizer.
 * @author Emeric Vernat
 */
public class TestJavaHTMLizer {
	/**
	 * Test.
	 */
	@Test
	public void test() {
		final String javaSource = "public class Test { static final String TEST = \"test\"; }";
		final String html = JavaHTMLizer.htmlizeFull(javaSource);
		assertTrue("htmlizeFull", html != null && !html.isEmpty());
	}
}

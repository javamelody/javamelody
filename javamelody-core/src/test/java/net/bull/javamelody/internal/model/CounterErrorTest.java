package net.bull.javamelody.internal.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.bull.javamelody.Utils;

/**
 * Created by zvrablik on 4/22/16.
 */
public class CounterErrorTest {
	/**
	 * Init.
	 */
	@BeforeEach
	public void setUp() {
		Utils.initialize();
	}

	/**
	 * Test.
	 */
	@Test
	public void testMessageAndStackTraceLengthSmall() {
		final String message = "aaaaaaaaaa";
		final String stackTrace = "bbbbbbbb";
		final CounterError counterError = new CounterError(message, stackTrace);
		assertEquals("message", message, counterError.getMessage());
		assertEquals("stackTrace", stackTrace, counterError.getStackTrace());
	}

	/**
	 * Test.
	 */
	@Test
	public void testMessageAndStackTraceLengthBig() {
		final int messageMaxLength = 1000;
		final int stackTraceMaxLength = 50000;
		final StringBuilder message = new StringBuilder();
		final StringBuilder stackTrace = new StringBuilder();
		for (int i = 0; i < messageMaxLength + 1; i++) {
			message.append('a');
		}
		for (int i = 0; i < stackTraceMaxLength + 1; i++) {
			stackTrace.append('b');
		}
		final CounterError counterError = new CounterError(message.toString(),
				stackTrace.toString());
		assertEquals(messageMaxLength, counterError.getMessage().length(), "message length");
		assertEquals(stackTraceMaxLength, counterError.getStackTrace().length(), "stackTrace length");
	}
}

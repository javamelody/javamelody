package net.bull.javamelody;

import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.*;

/**
 * Created by zvrablik on 4/22/16.
 */
public class CounterErrorTest {
    @Before
    public void setUp() {
        Utils.initialize();
    }

    @Test
    public void testMessageAndStackTraceSize() throws IOException {
        String message = "aaaaaaaaaa";
        String stackTrace = "bbbbbbbb";
        CounterError counterError = new CounterError(message, stackTrace);
        assertEquals(message, counterError.getMessage());
        assertEquals(stackTrace, counterError.getStackTrace());
    }

    @Test
    public void testMessageAndStackTraceSize4() throws IOException {
        String message = "aaaaaaaaaa";
        Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX+"log.size.message", "4");
        Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX+"log.size.stacktrace", "5");

        String stackTrace = "bbbbbbbb";
        CounterError counterError = new CounterError(message, stackTrace);
        String expectedMessage = "aaaa";
        assertEquals(expectedMessage, counterError.getMessage());

        String expectedStackTrace = "bbbbb";
        assertEquals(expectedStackTrace, counterError.getStackTrace());
    }

    @Test
    public void testMessageAndStackTraceSizeInvalidValues() throws IOException {
        String message = "aaaaaaaaaa";
        Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX+"log.size.message", "a4");
        Utils.setProperty(Parameters.PARAMETER_SYSTEM_PREFIX+"log.size.stacktrace", "a5");

        String stackTrace = "bbbbbbbb";
        CounterError counterError = new CounterError(message, stackTrace);
        assertEquals(message, counterError.getMessage());

        assertEquals(stackTrace, counterError.getStackTrace());
    }
}
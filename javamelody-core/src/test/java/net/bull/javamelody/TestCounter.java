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
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import net.bull.javamelody.Counter;
import net.bull.javamelody.CounterError;
import net.bull.javamelody.CounterRequest;
import net.bull.javamelody.CounterRequestContext;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


/**
 * Test unitaire de la classe Counter.
 * @author Emeric Vernat
 */
public class TestCounter {
	private Counter counter;

	/** Initialisation. */
	@Before
	public void setUp() {
		counter = new Counter("test", null);
		counter.bindContext("bind context", "bind my context");
	}

	/** Finalisation. */
	@After
	public void tearDown() {
		counter.unbindContext();
	}

	private CounterRequest createCounterRequest() {
		final CounterRequest request = new CounterRequest("test Counter", counter.getName());
		request.addHit(100, 50, false, 1000);
		return request;
	}

	/** Test. */
	@Test
	public void testAddRequest() {
		final CounterRequest request = createCounterRequest();
		// ce bindContext pour tester le cas où une requête est ajoutée avec un contexte et un contexte parent
		// puis une requête ajoutée avec un contexte sans contexte parent
		counter.bindContext(request.getName(), request.getName());

		counter.addRequest(request.getName(), request.getMean(), 0, false, request
				.getResponseSizeMean());
		final List<CounterRequest> before = counter.getOrderedRequests();
		counter.addRequest(request.getName(), request.getMean(), 0, false, request
				.getResponseSizeMean());
		counter.setRequestTransformPattern(Pattern.compile("aaaaa"));
		counter.addRequest(request.getName(), request.getMean(), 0, false, request
				.getResponseSizeMean());
		final List<CounterRequest> after = counter.getOrderedRequests();
		after.get(0).removeHits(request);
		after.get(0).removeHits(request);
		// on teste le contenu des CounterRequest par le contenu de toString
		assertEquals("requests", before.toString(), after.toString());

		// test addChildRequest dans addRequest
		final Counter sqlCounter = new Counter("sql", null);
		final Counter httpCounter = new Counter("http", null, sqlCounter);
		httpCounter.bindContext("http request", "http request");
		sqlCounter.bindContext("sql request", "sql request");
		sqlCounter.addRequest("sql request", 0, 0, false, -1); // ici context.addChildRequest
		sqlCounter.addRequest("sql request", 0, 0, false, -1); // 2ème pour passer dans le else de addChildRequestForDrillDown
		httpCounter.addRequest("http request", 10, 2, false, 100);
	}

	/** Test. */
	@Test
	public void testAddRequestForSystemError() {
		final CounterRequest request = createCounterRequest();
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		errorCounter.setMaxRequestsCount(200);
		errorCounter.addRequestForSystemError(request.getName(), request.getMean(), 0, null);
		final List<CounterRequest> before = errorCounter.getOrderedRequests();
		errorCounter
				.addRequestForSystemError(request.getName(), request.getMean(), 0, "stacktrace");
		final List<CounterRequest> after = errorCounter.getOrderedRequests();
		after.get(0).removeHits(request);
		// on teste le contenu des CounterRequest par le contenu de toString
		assertEquals("error requests", before.toString(), after.toString());
		int i = 0;
		while (errorCounter.getRequestsCount() < Counter.MAX_ERRORS_COUNT) {
			errorCounter.addRequestForSystemError("request " + i, 1, 0, null);
			i++;
		}
		errorCounter.addRequestForSystemError("request " + i, 1, 0, null);
	}

	/** Test. */
	@Test
	public void testAddHits() {
		final CounterRequest counterRequest = createCounterRequest();
		counter.addHits(counterRequest);
		final List<CounterRequest> before = counter.getOrderedRequests();
		counter.addHits(counterRequest);
		counter.addHits(new CounterRequest("test", counter.getName()));
		final List<CounterRequest> after = counter.getOrderedRequests();
		after.get(0).removeHits(counterRequest);
		// on teste le contenu des CounterRequest par le contenu de toString
		assertEquals("requests", before.toString(), after.toString());
		// on remet counterRequests.getHits() à 0
		counterRequest.removeHits(counterRequest);
		// on teste l'ajout de hits avec counterRequests à 0 hit(s)
		counter.addHits(counterRequest);
	}

	/** Test. */
	@Test
	public void testRemoveRequest() {
		final int count = counter.getRequestsCount();
		counter.addRequest("remove request", 100, 50, false, 1000);
		counter.removeRequest("remove request");
		assertEquals("requests count", count, counter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testAddRequests() {
		final CounterRequest counterRequest = createCounterRequest();
		counter.addHits(counterRequest);
		counter.bindContext("context", "context");
		final CounterRequest counterRequestWithoutHits = counter.getCounterRequest(counter
				.getOrderedRootCurrentContexts().get(0));
		counter.addHits(counterRequestWithoutHits);
		final List<CounterRequest> before = counter.getOrderedRequests();
		// ajout d'une instance de compteur avec requêtes qui ont des hits ou pas
		counter.addRequestsAndErrors(counter);
		final List<CounterRequest> after = counter.getOrderedRequests();
		after.get(0).removeHits(counterRequest);
		// on teste le contenu des CounterRequest par le contenu de toString
		assertEquals("requests", before.toString(), after.toString());

		// test de la limitation à maxRequestsCount dans l'ajout de requêtes
		final Counter counter2 = new Counter(counter.getName(), null);
		counter2.addRequest("request 2", 100, 50, false, 100);
		for (int i = 0; i < 20; i++) {
			counter2.addRequest("request 3", 100, 50, false, 100);
		}
		counter.setMaxRequestsCount(1);
		counter.addRequestsAndErrors(counter2);
		assertEquals("count", 1, counter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testAddErrors() {
		final CounterError beforeError = new CounterError("before", null);
		try {
			Thread.sleep(50);
		} catch (final InterruptedException e) {
			throw new IllegalStateException(e);
		}
		final Counter errorCounter = new Counter(Counter.ERROR_COUNTER_NAME, null);
		CounterError.bindRequest(null);
		CounterError.unbindRequest();
		final int errorsCount = errorCounter.getErrorsCount();
		final List<CounterError> errors = new ArrayList<CounterError>();
		errors.add(new CounterError("erreur", null));
		errors.add(new CounterError("erreur", "stacktrace"));
		errorCounter.addErrors(errors);
		errors.add(0, beforeError);
		errorCounter.addErrors(errors);
		assertEquals("addErrors", errorsCount + 5, errorCounter.getErrorsCount());
		while (errorCounter.getErrorsCount() < Counter.MAX_ERRORS_COUNT) {
			errorCounter.addErrors(errors);
		}
		errorCounter.addErrors(errors);
	}

	/** Test. */
	@Test
	public void testGetErrors() {
		assertTrue("getErrors", counter.getErrors().isEmpty());
		assertNotNull("getErrors", new Counter(Counter.ERROR_COUNTER_NAME, null).getErrors());
	}

	/** Test. */
	@Test
	public void testClear() {
		counter.addRequest("test clear", 100, 50, false, 1000);
		counter.clear();
		assertEquals("requests count", 0, counter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testGetCounterRequest() {
		counter.unbindContext();
		final String requestName = "get counter request";
		counter.bindContext(requestName, "my context");
		final CounterRequest counterRequest = counter.getCounterRequest(counter
				.getOrderedRootCurrentContexts().get(0));
		assertEquals("request name", requestName, counterRequest.getName());
	}

	/** Test. */
	@Test
	public void testGetOrderedRequests() {
		counter.clear();
		counter.addRequest("test a", 0, 0, false, 1000);
		counter.addRequest("test b", 1000, 500, false, 1000); // supérieur
		counter.addRequest("test c", 1000, 500, false, 1000); // égal
		counter.addRequest("test d", 100, 50, false, 1000); // inférieur
		final List<CounterRequest> requests = counter.getOrderedRequests();
		assertEquals("requests size", 4, requests.size());
	}

	/** Test. */
	@Test
	public void testGetOrderedByHitsRequests() {
		counter.clear();
		counter.addRequest("test 1", 0, 0, false, 1000);
		counter.addRequest("test 2", 1000, 500, false, 1000); // supérieur en hits
		counter.addRequest("test 2", 1000, 500, false, 1000);
		counter.addRequest("test 2", 1000, 500, false, 1000);
		counter.addRequest("test 3", 100, 50, false, 1000); // égal
		counter.addRequest("test 4", 100, 50, false, 1000); // inférieur
		counter.addRequest("test 4", 100, 50, false, 1000);
		final List<CounterRequest> requests = counter.getOrderedByHitsRequests();
		assertEquals("requests size", 4, requests.size());
	}

	/** Test. */
	@Test
	public void testGetOrderedRootCurrentContexts() {
		counter.unbindContext();
		final String requestName = "root context";

		final int nbRootContexts = 100; // 100 pour couvrir tous les cas du compartor de tri
		bindRootContexts(requestName, counter, nbRootContexts);
		// addRequest pour rentrer dans le if de la map dans CounterRequestContext.clone
		counter.addRequest(requestName, 100, 100, false, 1000);

		final List<CounterRequestContext> rootContexts = counter.getOrderedRootCurrentContexts();
		assertEquals("contexts size", nbRootContexts + 1, rootContexts.size());
		assertEquals("context name", requestName, rootContexts.get(0).getRequestName());

		final String string = rootContexts.get(0).toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
	}

	static void bindRootContexts(String firstRequestName, Counter myCounter, int nbRootContexts) {
		myCounter.bindContext(firstRequestName, "my context");
		myCounter.bindContext("child context", "my child context");
		// on crée d'autres racines de contexte
		try {
			Thread.sleep(100);
			for (int i = 0; i < nbRootContexts; i++) {
				bindRootContext(myCounter).join(1000);
			}
		} catch (final InterruptedException e) {
			fail(e.toString());
		}
	}

	private static Thread bindRootContext(final Counter myCounter) { // NOPMD
		final Thread thread = new Thread(new Runnable() { // NOPMD
					public void run() {
						myCounter.bindContext("second root context", "my context");
					}
				});
		thread.setDaemon(true);
		thread.start();
		return thread;
	}

	/** Test. */
	@Test
	public void testGetRequests() {
		counter.clear();
		final CounterRequest counterRequest = createCounterRequest();
		counter.addHits(counterRequest);
		final List<CounterRequest> requests = counter.getRequests();
		assertEquals("requests size", 1, requests.size());
		assertEquals("request", counterRequest.toString(), requests.get(0).toString());
	}

	/** Test. */
	@Test
	public void testGetRequestsCount() {
		counter.addRequest("test requests count", 100, 50, false, 1000);
		assertEquals("requests count", counter.getRequests().size(), counter.getRequestsCount());
	}

	/** Test. */
	@Test
	public void testApplication() {
		final String value = "app";
		assertNotSame(value, counter.getApplication());
		counter.setApplication(value);
		assertSame("application", value, counter.getApplication());
	}

	/** Test. */
	@Test
	public void testRequestTransformPattern() {
		final Pattern value = Pattern.compile("a*");
		assertNotSame(value, counter.getRequestTransformPattern());
		counter.setRequestTransformPattern(value);
		assertSame("request transform pattern", value, counter.getRequestTransformPattern());
	}

	/** Test. */
	@Test
	public void testStartDate() {
		final Date value = new Date(System.currentTimeMillis() + 1000);
		assertNotSame(value, counter.getStartDate());
		counter.setStartDate(value);
		assertSame("start date", value, counter.getStartDate());
	}

	/** Test. */
	@Test
	public void testDisplayed() {
		final boolean value = false;
		assertNotSame(value, counter.isDisplayed());
		counter.setDisplayed(value);
		assertSame("displayed", value, counter.isDisplayed());
	}

	/** Test. */
	@Test
	public void testErrorCounter() {
		assertFalse("errorCounter", new Counter("http", null).isErrorCounter());
		assertTrue("errorCounter", new Counter("error", null).isErrorCounter());
		assertTrue("errorCounter", new Counter("log", null).isErrorCounter());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testReadFromFile() throws IOException {
		// test pour un counter vide
		counter.clear();
		counter.writeToFile();
		counter.readFromFile();
		// test pour un counter non vide
		final CounterRequest counterRequest = createCounterRequest();
		counter.addHits(counterRequest);
		counter.writeToFile();
		// readFromFile ajoute les requêtes lues aux requêtes actuelles
		counter.readFromFile();
		assertEquals("request hits", counterRequest.getHits() * 2, counter.getRequests().get(0)
				.getHits());
		counter.clear();
		counter.readFromFile();
		assertEquals("request", counterRequest.toString(), counter.getRequests().get(0).toString());
	}

	/** Test.
	 * @throws IOException e */
	@Test
	public void testWriteToFile() throws IOException {
		// test pour un counter vide
		counter.clear();
		counter.writeToFile();
		// test pour un counter non vide
		counter.addRequest("test writeToFile", 100, 50, false, 1000);
		final String before = counter.toString();
		counter.writeToFile();
		assertEquals("counter", before, counter.toString());
	}

	/** Test. */
	@Test
	public void testToString() {
		counter.addRequest("test toString", 100, 50, false, 1000);
		final String string = counter.toString();
		assertNotNull("toString not null", string);
		assertFalse("toString not empty", string.isEmpty());
		final String string2 = new Counter(Counter.ERROR_COUNTER_NAME, null).toString();
		assertNotNull("toString not null", string2);
		assertFalse("toString not empty", string2.isEmpty());
		if (createCounterRequest().toString().isEmpty()) {
			fail("toString vide");
		}
	}
}

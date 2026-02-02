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
package net.bull.javamelody;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.bson.Document;
import org.bson.codecs.configuration.CodecRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

/**
 * Test unitaire de la classe MongoWrapper.
 * @author Emeric Vernat
 */
class TestMongoWrapper {
	/** Test. */
	@BeforeEach
	void setUp() {
		Utils.initialize();
	}

	/** Test. */
	@Test
	void testCreateDatabaseProxy() {
		final MongoDatabase database = createNiceMock(MongoDatabase.class);
		final MongoCollection<Document> collection = createNiceMock(MongoCollection.class);
		final CodecRegistry codecRegistry = createNiceMock(CodecRegistry.class);
		expect(database.withCodecRegistry(codecRegistry)).andReturn(database).anyTimes();
		expect(database.getCollection("collection")).andReturn(collection).anyTimes();
		expect(database.getName()).andReturn("db").anyTimes();
		expect(collection.withCodecRegistry(codecRegistry)).andReturn(collection).anyTimes();

		replay(database);
		replay(collection);
		replay(codecRegistry);
		final MongoDatabase databaseProxy = MongoWrapper.createDatabaseProxy(database);
		assertNotNull(databaseProxy, "createDatabaseProxy");
		assertNotNull(databaseProxy.withCodecRegistry(codecRegistry), "databaseProxy");
		assertNotNull(databaseProxy.getCollection("collection"), "getCollection");
		assertEquals("db", databaseProxy.getName(), "getName");

		assertNotNull(databaseProxy.getCollection("collection").withCodecRegistry(codecRegistry), "collectionProxy");
		databaseProxy.getCollection("collection").getCodecRegistry();
		databaseProxy.getCollection("collection").find();
		verify(database);
		verify(collection);
		verify(codecRegistry);
	}
}

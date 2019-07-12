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

import java.sql.SQLException;
import java.util.Date;

/**
 * Interface test de bean Spring.
 * @author Emeric Vernat
 */
public interface SpringTestFacade {
	/**
	 * Méthode test de service sur la façade.
	 * @return Date
	 * @throws SQLException e
	 */
	Date nowWithSql() throws SQLException;

	/**
	 * Méthode test de service sur la façade.
	 * @return Date
	 */
	Date now();

	/**
	 * Méthode test de service sur la façade.
	 */
	void throwError();

	/**
	 * Méthode test de service sur la façade.
	 */
	void throwException();
}

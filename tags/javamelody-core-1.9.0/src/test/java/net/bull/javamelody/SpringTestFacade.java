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

import java.util.Date;

/**
 * Interface test de bean Spring.
 * @author Emeric Vernat
 */
public interface SpringTestFacade {
	/**
	 * Méthode test de service sur la façade.
	 * @return Date
	 * @throws Exception e
	 */
	Date nowWithSql() throws Exception;

	/**
	 * Méthode test de service sur la façade.
	 * @return Date
	 * @throws Exception e
	 */
	Date now() throws Exception;

	/**
	 * Méthode test de service sur la façade.
	 */
	void throwError();
}

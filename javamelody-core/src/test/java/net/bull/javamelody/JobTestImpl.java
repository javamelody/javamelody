/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.util.Random;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

/**
 * Implementation test d'un job quartz.
 * @author Emeric Vernat
 */
public class JobTestImpl implements Job {
	private static final Random RANDOM = new Random();

	/** {@inheritDoc} */
	public void execute(JobExecutionContext arg0) throws JobExecutionException {
		try {
			Thread.sleep(RANDOM.nextInt(2000));
		} catch (final InterruptedException e) {
			throw new JobExecutionException(e);
		}
		if (RANDOM.nextInt(10) >= 8) {
			throw new JobExecutionException("il y a une erreur");
		}
	}
}

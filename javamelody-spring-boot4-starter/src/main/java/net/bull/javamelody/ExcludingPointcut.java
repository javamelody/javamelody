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

import java.lang.reflect.Method;

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;

/**
 * Pointcut composed of a delegate pointcut and an excluding pointcut.
 * @author Emeric Vernat
 */
class ExcludingPointcut implements Pointcut {
	private final Pointcut delegatePointcut;
	private Pointcut excludingPointcut;

	private final MethodMatcher methodMatcher = new MethodMatcher() {
		@Override
		public boolean isRuntime() {
			return getDelegatePointcut().getMethodMatcher().isRuntime()
					|| getExcludingPointcut().getMethodMatcher().isRuntime();
		}

		@Override
		public boolean matches(Method method, Class<?> targetClass) {
			return getDelegatePointcut().getMethodMatcher().matches(method, targetClass)
					&& !(getExcludingPointcut().getClassFilter().matches(targetClass)
							&& getExcludingPointcut().getMethodMatcher().matches(method,
									targetClass));
		}

		@Override
		public boolean matches(Method method, Class<?> targetClass, Object... args) {
			return getDelegatePointcut().getMethodMatcher().matches(method, targetClass, args)
					&& !(getExcludingPointcut().getClassFilter().matches(targetClass)
							&& getExcludingPointcut().getMethodMatcher().matches(method,
									targetClass, args));
		}
	};

	ExcludingPointcut(Pointcut delegatePointcut) {
		super();
		assert delegatePointcut != null;
		this.delegatePointcut = delegatePointcut;
	}

	Pointcut exclude(Pointcut pointcut) {
		assert pointcut != null;
		this.excludingPointcut = pointcut;
		return this;
	}

	Pointcut getDelegatePointcut() {
		assert delegatePointcut != null;
		return delegatePointcut;
	}

	Pointcut getExcludingPointcut() {
		assert excludingPointcut != null;
		return excludingPointcut;
	}

	@Override
	public ClassFilter getClassFilter() {
		return delegatePointcut.getClassFilter();
	}

	@Override
	public MethodMatcher getMethodMatcher() {
		return methodMatcher;
	}
}

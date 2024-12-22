package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Form;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FormRepository extends JpaRepository<Form, Long> {
}

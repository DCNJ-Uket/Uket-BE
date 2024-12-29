package com.uket.domain.form.repository;

import com.uket.domain.form.entity.Options;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface OptionsRepository extends JpaRepository<Options, Long> {
    List<Options> findByFormId(Long formId);
}

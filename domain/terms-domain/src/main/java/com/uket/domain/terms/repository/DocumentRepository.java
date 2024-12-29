package com.uket.domain.terms.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.uket.domain.terms.entity.Document;

public interface DocumentRepository extends JpaRepository<Document, Long> {

}

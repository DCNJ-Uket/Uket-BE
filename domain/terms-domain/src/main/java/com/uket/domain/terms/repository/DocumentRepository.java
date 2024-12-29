package com.uket.domain.terms.repository;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import com.uket.domain.terms.entity.Document;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface DocumentRepository extends JpaRepository<Document, Long> {

    @Query("""
        SELECT d
        FROM Document d
        WHERE d.documentNo IN :documentNos
          AND d.version = (
              SELECT MAX(subD.version)
              FROM Document subD
              WHERE subD.documentNo = d.documentNo
          )
    """)
    List<Document> findLatestDocumentsByDocumentNos(@Param("documentNos") List<Long> documentNos);
}

package com.uket.domain.terms.service;

import com.uket.domain.terms.entity.Document;
import com.uket.domain.terms.repository.DocumentRepository;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class DocumentService {

    private final DocumentRepository documentRepository;

    @Transactional(readOnly = true)
    public Map<Long, String> getLinkMap(List<Long> documentNos) {
        return documentRepository.findLatestDocumentsByDocumentNos(documentNos).stream()
                .collect(Collectors.toMap(Document::getDocumentNo, Document::getLink));
    }
}

package com.uket.domain.event.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.ShowNameDto;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.ShowRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ShowService {

    private final ShowRepository showRepository;

    public List<ShowDto> findByEventId(Long eventId) {
        return showRepository.findByEventId(eventId);
    }

    public String findNameById(Long showId) {
        ShowNameDto showNameDto = showRepository.findNameById(showId)
                .orElseThrow(() -> new EventException(ErrorCode.NOT_FOUND_SHOW));

        return showNameDto.name();
    }
}

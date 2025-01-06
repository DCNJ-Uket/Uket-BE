CREATE TABLE IF NOT EXISTS survey (
                                      survey_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                      created_at DATETIME(6) NOT NULL,
                                      modified_at DATETIME(6) NOT NULL
);

CREATE TABLE IF NOT EXISTS form (
                                    form_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                    created_at DATETIME(6) NOT NULL,
                                    modified_at DATETIME(6) NOT NULL,
                                    form_type VARCHAR(255) NOT NULL,
                                    max_length INT DEFAULT NULL,
                                    question VARCHAR(255) NOT NULL,
                                    survey_id BIGINT NOT NULL,
                                    is_necessary BIT(1) NOT NULL
);

CREATE TABLE IF NOT EXISTS options (
                                       option_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                       created_at DATETIME(6) NOT NULL,
                                       modified_at DATETIME(6) NOT NULL,
                                       value VARCHAR(255) NOT NULL,
                                       form_id BIGINT NOT NULL
);

CREATE TABLE IF NOT EXISTS answer (
                                      answer_id BIGINT AUTO_INCREMENT PRIMARY KEY,
                                      created_at DATETIME(6) NOT NULL,
                                      modified_at DATETIME(6) NOT NULL,
                                      response VARCHAR(255) NOT NULL,
                                      form_id BIGINT NOT NULL,
                                      user_id BIGINT NOT NULL
);


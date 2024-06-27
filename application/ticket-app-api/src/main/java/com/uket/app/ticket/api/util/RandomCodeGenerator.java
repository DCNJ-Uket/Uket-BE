package com.uket.app.ticket.api.util;

import java.util.Random;

public class RandomCodeGenerator {

    private static final Random random = new Random();
    private static final int CODE_LENGTH = 5;
    private static final int MIN = (int) Math.pow(10, CODE_LENGTH);
    private static final int MAX = 9 * MIN;

    public static String generateRandomCode() {
        return Integer.toString(random.nextInt(MAX) + MIN);
    }
}

import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.5.4/index.ts';
import { assertEquals } from 'https://deno.land/std@0.170.0/testing/asserts.ts';

Clarinet.test({
    name: "Health Metrics Tracker: Validate core functionality",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const deployer = accounts.get('deployer')!;
        const user = accounts.get('wallet_1')!;

        // Test recording a health metric
        let block = chain.mineBlock([
            Tx.contractCall(
                'health-metrics-tracker', 
                'record-health-metric', 
                [
                    types.uint(1), // metric type (pulse)
                    types.uint(75), // value
                    types.uint(chain.blockHeight), // timestamp
                    types.some(types.utf8('Morning reading'))
                ],
                user.address
            )
        ]);

        // Verify record was successful
        assertEquals(block.receipts[0].result, '(ok true)');

        // Test retrieving latest measurement
        let latestMeasurement = chain.callReadOnlyFn(
            'health-metrics-tracker', 
            'get-latest-measurement', 
            [types.principal(user.address), types.uint(1)],
            user.address
        );

        // Validate retrieval
        assertEquals(
            latestMeasurement.result, 
            '(ok (some {value: u75, notes: (some "Morning reading")}))' 
        );
    }
});

Clarinet.test({
    name: "Health Metrics Tracker: Validate input validation",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const user = accounts.get('wallet_1')!;

        // Test recording invalid metric type
        let invalidBlock = chain.mineBlock([
            Tx.contractCall(
                'health-metrics-tracker', 
                'record-health-metric', 
                [
                    types.uint(99), // invalid metric type
                    types.uint(75),
                    types.uint(chain.blockHeight),
                    types.none()
                ],
                user.address
            )
        ]);

        // Verify error handling for invalid type
        assertEquals(invalidBlock.receipts[0].result, '(err u101)');  // ERR-INVALID-METRIC-TYPE
    }
});
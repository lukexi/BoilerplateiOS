//
//  BLPMainViewController.m
//  Boilerplate
//
//  Created by Luke Iannini on 4/9/14.
//  Copyright (c) 2014 Tree. All rights reserved.
//

#import "BLPMainViewController.h"
#define HsPtr id
#import "Main_stub.h"

#define BLPCellSize 44

typedef NS_ENUM(NSUInteger, BLPCellType) {
    BLPGround = 0,
    BLPNothingness,
    BLPShuttle,
    BLPThinShuttle,
    BLPThinSolid,
    BLPBridge,
    BLPPositive,
    BLPNegative
};

UIColor *colorForPressure(NSInteger pressure) {
    if (pressure < 0) {
        return [UIColor colorWithRed:1 green:0.8 blue:0.8 alpha:1];
    } else if (pressure > 0) {
        return [UIColor colorWithRed:0.81 green:1 blue:0.81 alpha:1];
    }
    return nil;
};

UIColor *colorForCellType(BLPCellType cellType) {
    switch (cellType) {
        case BLPNothingness:
            return [UIColor whiteColor];
            break;
        case BLPShuttle:
            return [UIColor colorWithRed:0.53 green:0.06 blue:0.53 alpha:1];
            break;
        case BLPThinShuttle:
            return [UIColor colorWithRed:0.8 green:0.24 blue:0.79 alpha:1];
            break;
        case BLPThinSolid:
            return [UIColor colorWithRed:0.53 green:0.53 blue:0.53 alpha:1];
            break;
        case BLPBridge:
            return [UIColor colorWithRed:0.09 green:0.55 blue:0.99 alpha:1];
            break;
        case BLPPositive:
            return [UIColor colorWithRed:0.16 green:1 blue:0.18 alpha:1];
            break;
        case BLPNegative:
            return [UIColor colorWithRed:0.99 green:0.05 blue:0.11 alpha:1];
            break;
        default:
            break;
    }
    return [UIColor blackColor];
}

@interface BLPMainViewController () <UIGestureRecognizerDelegate>

@end

@implementation BLPMainViewController {
    NSDictionary *simulation;
    NSMutableArray *cellViews;
    IBOutlet UISegmentedControl *palette;
    NSInteger currentX;
    NSInteger currentY;
    NSTimer *stepTimer;
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (BOOL)prefersStatusBarHidden {
    return YES;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    cellViews = [NSMutableArray array];
    
    hs_init(NULL, NULL);
    simulation = @{@[@0,@0]: @(BLPPositive),
                   @[@1,@0]: @(BLPShuttle),
                   @[@2,@0]: @(BLPNothingness),
                   @[@3,@0]: @(BLPNothingness),
                   @[@4,@0]: @(BLPNothingness),
                   @[@5,@0]: @(BLPNothingness),
                   @[@6,@0]: @(BLPNothingness),
                   @[@7,@0]: @(BLPNothingness),
                   @[@8,@0]: @(BLPNothingness),
                   @[@5,@3]: @(BLPNegative),
                   @[@5,@4]: @(BLPShuttle),
                   @[@6,@3]: @(BLPNothingness),
                   @[@6,@4]: @(BLPNothingness),
                   };
    stepTimer = [NSTimer scheduledTimerWithTimeInterval:0.1 target:self selector:@selector(step) userInfo:nil repeats:YES];
    
    UILongPressGestureRecognizer *dragReco = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(drag:)];
    dragReco.cancelsTouchesInView = NO;
    dragReco.minimumPressDuration = 0;
    dragReco.delegate = self;
    [self.view addGestureRecognizer:dragReco];
}

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch {
    return touch.view == self.view;
}

- (void)drag:(UILongPressGestureRecognizer *)drag {
    
    CGPoint loc = [drag locationInView:self.view];
    NSInteger x = loc.x / BLPCellSize;
    NSInteger y = loc.y / BLPCellSize;
    if (drag.state == UIGestureRecognizerStateBegan || x != currentX || y != currentY) {
        simulation = simSet(@[@(x), @(y)],
                            palette.selectedSegmentIndex == BLPGround ? nil : @(palette.selectedSegmentIndex),
                            simulation);
        currentX = x;
        currentY = y;
    }
}

- (void)step {
    [cellViews makeObjectsPerformSelector:@selector(removeFromSuperview)];
    [cellViews removeAllObjects];
    
    NSDictionary *pressure = simPressure(simulation);
    for (NSArray *point in simulation) {
        NSInteger x = [point[0] integerValue];
        NSInteger y = [point[1] integerValue];
        NSInteger type = [simulation[point] integerValue];
        
        UIView *view = [[UIView alloc] initWithFrame:CGRectMake(BLPCellSize * x, BLPCellSize * y, BLPCellSize, BLPCellSize)];
        view.backgroundColor = colorForCellType(type);
        
        NSNumber *pressureForPoint = pressure[point];
        if (type == BLPNothingness && pressureForPoint) {
            view.backgroundColor = colorForPressure(pressureForPoint.integerValue);
        }
        
        view.userInteractionEnabled = NO;
        [self.view insertSubview:view atIndex:0];
        [cellViews addObject:view];
    }
    
    simulation = simStep(simulation);
}


@end

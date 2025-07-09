#!/usr/bin/env python3
import sys
import re
import subprocess
import os
from PIL import Image, ImageDraw, ImageFont

def extract_numbers(s: str):
    nums = re.findall(r"\d+", s)
    seen = set(); out = []
    for n in nums:
        if n not in seen:
            seen.add(n); out.append(n)
    return out

def flybase_url(bdsc_number: str) -> str:
    padded = bdsc_number.zfill(7)
    return f"https://flybase.org/reports/FBst{padded}"

QR_SCRIPT = os.path.join(os.path.dirname(__file__), 'qr.py')
FONT_PATH = 'Consolas.ttf'  # path to your .ttf file
PADDING = 400               # space between text and QR
TEXT_HEIGHT_RATIO = 0.5     # text height as a fraction of QR height

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python bdsc.no2qr.py <BDSC numbers separated by commas/spaces>")
        sys.exit(1)

    raw = " ".join(sys.argv[1:])
    nums = extract_numbers(raw)
    if not nums:
        print("No BDSC numbers found!")
        sys.exit(1)

    for n in nums:
        url = flybase_url(n)
        filename = f"BDSC_{n}.png"
        print(f"> Generating QR for BDSC {n} → {filename} (URL: {url})")
        subprocess.run([sys.executable, QR_SCRIPT, url, filename], check=True)

        img_path = os.path.join('qr', filename)
        qr_img = Image.open(img_path)
        qr_w, qr_h = qr_img.size

        text = f"BDSC:{n}"
        # load a base font to measure
        try:
            base_size = 20
            base_font = ImageFont.truetype(FONT_PATH, base_size)
        except IOError:
            base_font = ImageFont.load_default()
            base_size = getattr(base_font, 'size', 12)

        draw = ImageDraw.Draw(qr_img)
        bbox = draw.textbbox((0, 0), text, font=base_font)
        base_text_h = bbox[3] - bbox[1]

        # compute scaled font size for desired TEXT_HEIGHT_RATIO
        target_h = int(qr_h * TEXT_HEIGHT_RATIO)
        if base_text_h > 0:
            scale = target_h / base_text_h
            new_font_size = max(1, int(base_size * scale))
        else:
            new_font_size = base_size

        try:
            font = ImageFont.truetype(FONT_PATH, new_font_size)
        except IOError:
            font = ImageFont.load_default()

        # measure text size with scaled font
        draw_temp = ImageDraw.Draw(qr_img)
        bbox = draw_temp.textbbox((0, 0), text, font=font)
        text_w = bbox[2] - bbox[0]
        text_h = bbox[3] - bbox[1]

        # create new canvas: height = QR height, width = text + padding + QR width
        new_w = text_w + PADDING + qr_w
        new_h = qr_h
        new_img = Image.new('RGB', (new_w, new_h), 'white')

        # draw text on the left, vertically centered
        draw2 = ImageDraw.Draw(new_img)
        text_x = 0
        text_y = ((new_h - text_h) // 2)*0.8
        draw2.text((text_x, text_y), text, font=font, fill='black')

        # paste QR to the right of the text, vertically top-aligned
        qr_x = text_w + PADDING
        new_img.paste(qr_img, (qr_x, 0))

        # save back
        new_img.save(img_path)
        print(f"Overlayed text '{text}' with QR on right in {filename} (font size {getattr(font, 'size', 'default')})\n")

